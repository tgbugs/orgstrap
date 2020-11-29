;;; reval.el --- Remote elisp eval. -*- lexical-binding: t -*-

;; Author: Tom Gillespie
;; Homepage: https://github.com/tgbugs/orgstrap
;; Version: 9999
;; Package-Requires: ((emacs "24.4"))
;; Is-Version-Of: https://raw.githubusercontent.com/tgbugs/orgstrap/master/reval.el
;; Reval-Get-Immutable: reval--reval-update

;;;; License and Commentary

;; License:
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; reval.el implements eval of remote elisp code. More accurately
;; it implements local evaluation of remote elisp in a way that is
;; somewhat secure.

;;; Code:

(defcustom reval-default-cypher 'sha256
  "Default cypher to use to fill in a hole in `reval'."
  :type 'symbol
  :options (secure-hash-algorithms))

(defvar reval-failed-buffer-list nil "List of failed reval buffers.")

(defvar reval-evaled-buffer-list nil "List of evaled reval buffers.")

(defmacro with-url-handler-mode (&rest body)
  "Run BODY with `url-handler-mode' enabled."
  (declare (indent defun))
  `(let ((uhm url-handler-mode))
     (unwind-protect
         (progn
           (url-handler-mode)
           ,@body)
       (unless uhm
         (url-handler-mode 0)))))

(defun reval-id->buffer (path-or-url)
  "Given a PATH-OR-URL return a buffer of its contents."
  ;; We explicitly do not catch errors here since they need to
  ;; be caught by the human in the loop later.
  (with-url-handler-mode
    (find-file-noselect path-or-url)))

(defun reval-resum-review (cypher buffer &optional review)
  "Return checksum under CYPHER for BUFFER.
If REVIEW is non-nil then switch to BUFFER and prompt asking if audit
is ok before continuing."
  ;; we don't need to check or review alternates here because they
  ;; must all be identical
  (let (enable-local-eval)
    (save-window-excursion
      (with-current-buffer buffer
        (unless (file-exists-p (buffer-file-name))
          ;; NOTE url-handler-mode must be set in the calling context
          ;; this case should not happen, only extant files should make it here
          (error "reval-resum: file does not exist! %s" (buffer-file-name)))
        (when review
          (switch-to-buffer (current-buffer))
          ;; FIXME `yes-or-no-p' still blocks the command loop in >= 27 emacsclient
          (unless (yes-or-no-p "Audit of file ok? ") ; not using `y-or-n-p' since it is too easy
            (error "Audit failed. Checksum will not be calculated for %s"
                   (buffer-file-name (current-buffer)))))

        ;; need to ensure that file is actually elisp
        ;; note that in some cases read can succeed
        ;; even when a file is not elisp e.g. an html
        ;; file can sometimes read without error but
        ;; will fail on eval

        ;; elisp check by major mode
        (unless (eq major-mode 'emacs-lisp-mode)
          (error "Not an emacs lisp file!"))

        ;; elisp check by read
        (condition-case nil
            (read (concat "(progn\n"
                          (buffer-substring-no-properties (point-min) (point-max))
                          "\n)"))
          (error (error "Not an emacs lisp file!")))

        ;; return the checksum
        (intern (secure-hash cypher (current-buffer)))))))

(defun reval-resum-minimal (cypher buffer)
  "Checksum of BUFFER under CYPHER." ; minimal for maximal porability
  ;; not used since the expression takes up less space
  (intern (secure-hash cypher buffer)))

(defalias 'reval-resum #'reval-resum-review)

(defvar reval--make-audit t "Dynamic variable to control audit during `reval--make'")
;; the control of audit behavior is intentionally excluded from the
;; arguments of `reval--make' so that top level calls must audit
(defun reval--make (cypher path-or-url)
  "Make a `reval' expression from CYPHER and PATH-OR-URL.
This should not be used directly at the top level see docs for `reval'
for a better workflow."
  (unless reval--make-audit
    (warn "`reval--make' not auditing %S" path-or-url))
  (let ((checksum (reval-resum cypher (reval-id->buffer path-or-url) reval--make-audit)))
    `(reval ',cypher ',checksum ,path-or-url)))

(defun reval-audit ()
  "Audit the reval under the cursor."
  (interactive)
  (save-excursion
    (re-search-backward "(reval[[:space:]]")
    (let ((begin (point)))
      (forward-sexp)
      (let* ((raw (read (buffer-substring-no-properties begin (point))))
             (buffer (with-url-handler-mode
                       (find-file-noselect (elt raw 3))))
             (checksum (reval-resum (elt raw 1) buffer t)))
        (eq (elt raw 2) checksum)))))

(defun reval--add-buffer-to-list (buffer buffer-list-name)
  "Add BUFFER to list at BUFFER-LIST-NAME."
  (with-current-buffer buffer ; FIXME do this in both cases but change which list
    (push buffer (symbol-value buffer-list-name))
    ;; push first since it is better to have a dead buffer linger in a list
    ;; than it is to have an error happen during execution of `kill-buffer-hook'
    (let ((buffer-list-name buffer-list-name))
      (add-hook 'kill-buffer-hook
                (lambda ()
                  ;; read the manual for sets and lists to see why we have to
                  ;; setq here ... essentially if our element is found in the
                  ;; car of the list then the underlying list is not modified
                  ;; and the cdr of the list is returned, therefore if you have
                  ;; a list of all zeros and try to delete zero from it the list
                  ;; will remain unchanged unless you also setq the name to the
                  ;; (now) cdr value
                  (set buffer-list-name
                       (delete (current-buffer) (symbol-value buffer-list-name))))
                nil t))))

(defun reval (cypher checksum path-or-url &rest alternates)
  "Open PATH-OR-URL, match CHECKSUM under CYPHER, then eval.
If an error is encountered try ALTERNATES in order.

The simplest way to populate a `reval' expression starting from just
PATH-OR-URL is to write out expression with CYPHER and CHECKSUM as a
nonsense values. For example (reval ? ? \"path/to/file.el\"). Then run
M-x `reval-update-simple' to populate CYPHER and CHECKSUM."

  (let (done)
    (with-url-handler-mode
      (cl-loop for path-or-url in (cons path-or-url alternates)
               do (if (file-exists-p path-or-url)
                      (let* ((buffer (reval-id->buffer path-or-url))
                             ;; FIXME this is still not right ... can error due to not elisp
                             (buffer-checksum (reval-resum cypher buffer)))
                        (if (eq buffer-checksum checksum)
                            (progn
                              (eval-buffer buffer)
                              (reval--add-buffer-to-list buffer 'reval-evaled-buffer-list)
                              (setq done t))
                          (reval--add-buffer-to-list buffer 'reval-failed-buffer-list)
                          (funcall (if alternates #'warn #'error)
                                   ;; if alternates warn to prevent an early failure
                                   ;; from blocking later potential successes otherwise
                                   ;; signal an error
                                   "reval: checksum mismatch! %s" path-or-url)))
                    (warn "reval: file does not exist! %s" path-or-url))
               until done))
    (unless done
      (error "reval: all paths failed!")))
  nil)

(defun reval-view-failed ()
  "View top of failed reval buffer stack and kill or keep."
  (interactive)
  (when reval-failed-buffer-list
    (save-window-excursion
      (with-current-buffer (car reval-failed-buffer-list)
        (switch-to-buffer (current-buffer))
        (when (y-or-n-p "Kill buffer? ")
          (kill-buffer))))))

(require 'lisp-mnt)

(defvar url-http-end-of-headers)
(defun reval-url->json (url)  ; see utils.el
  "given a url string return json as a hash table"
  (json-parse-string
   (with-current-buffer (url-retrieve-synchronously url)
     (buffer-substring url-http-end-of-headers (point-max)))))

(defun reval--get-new-immutable-url ()
  (let ((get-imm-name (reval-header-get-immutable)))
    (if get-imm-name
        (let ((get-imm (intern get-imm-name)))
          (if (fboundp get-imm)
              (funcall get-imm)
            (warn "Function %s from Reval-Get-Immutable not found in %s" get-imm-name (buffer-file-name))
            nil))
      (warn "Reval-Get-Immutable: header not found in %s" (buffer-file-name))
      nil)))

(defun reval-get-imm-github (group repo path &optional branch)
  (let* ((branch (or branch "master"))
         (url (format "https://api.github.com/repos/%s/%s/git/refs/heads/%s" group repo branch))
         (sha (gethash "sha" (gethash "object" (reval-url->json url)))))
    (format "https://raw.githubusercontent.com/%s/%s/%s/%s" group repo branch path)))

(defun reval-header-is-version-of (&optional file)
  "Return the Is-Version-Of: header for FILE or current buffer."
  ;; this was originally called Latest-Version but matching the
  ;; datacite relationships seems to make more sense here esp.
  ;; since this is literally the example from the documentation
  (lm-with-file file
    (lm-header "is-version-of")))

(defun reval-header-get-immutable (&optional file)
  "Return the Reval-Get-Immutable: header for File or current buffer.

The value of this header should name a function in the current
file that returns an immutable name that points to the current
remote version of the the current file.

The implementation of the function may assume that the reval
package is present on the system."
  ;; there will always have to be a function because even if the
  ;; remote does all the work for us we will still have to ask the
  ;; remote to actually do the dereference operation, since we can't
  ;; gurantee that all remotes even have endpoints that behave this
  ;; way we can't implement this once in reval, so we ask individual
  ;; files to implement this pattern themselves, or worst case, the
  ;; update function can be supplied at update time if there is a
  ;; useful remote file that doesn't know that it is being revaled
  (lm-with-file file
    (lm-header "reval-get-immutable")))

(defun reval-check-for-updates () ; TODO reval-sync ?
  "Check current buffer revals for updates."
  (interactive)
  ;; search and collect all calls to reval in the current buffer? all org files? ???
  ;; open the current reval in a buffer
  ;; get the package info if avaiable
  ;; warn about all revals that cannot be updated due to missing metadata?

  (message "%S" )
  (error "TODO not implemented"))

(defun reval-update ()
  "Update to latest version."
  )

(defun reval--dquote-symbolp (thing)
  "Matches pattern ''THING.
Useful when dealing with quoted symbols in the outpub or a `read'.
For example elt 2 of '(reval 'sha256 ? \"file.el\")."
  (and (consp thing)
       (eq (car thing) 'quote)
       (consp (cdr thing))
       (symbolp (cadr thing))))

(defun reval-update-simple (&optional universal-argument)
  "Update the checksum for the reval sexp under the cursor or up the page.
Useful when developing against a local path or a mutable remote id.
If UNIVERSAL-ARGUMENT is non-nil then `reval-audit' is skipped, please use
this functionality responsibly."
  (interactive "P")
  (with-url-handler-mode
    (save-excursion
      (re-search-backward "(reval[[:space:]]")
      (let ((begin (point)))
        (forward-sexp)
        (let ((raw (read (buffer-substring-no-properties begin (point)))))
          ;;(message "aaaaa: %S %S %S" raw (symbolp (elt raw 1)) (type-of (elt raw 1)))
          (let ((cypher (let ((cy (elt raw 1)))
                          ;; '(sigh 'sigh) XXX the usual eval dangerzone >_<
                          (if (reval--dquote-symbolp cy) (eval cy) reval-default-cypher)))
                (checksum (let ((cs (elt raw 2)))
                            (if (reval--dquote-symbolp cs) (eval cs) nil)))
                (path-or-url (elt raw 3))
                (reval--make-audit (not universal-argument)))
            (unless (memq cypher (secure-hash-algorithms))
              (error "%S is not a known member of `secure-hash-algorithms'" cypher))
            (let ((new (reval--make cypher path-or-url))
                  (print-quoted t)
                  print-length
                  print-level)
              (backward-kill-sexp)
              (insert (prin1-to-string new)))))))))

(defun reval--reval-update ()
  "Get the immutable url for the current remote version of this file."
  (reval-get-imm-github "tgbugs" "orgstrap" "reval.el"))

(provide 'reval)

;;; reval.el ends here
