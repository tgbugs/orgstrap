;;; ow.el --- Common functionality for Orgware files. -*- lexical-binding: t -*-

;; Author: Tom Gillespie
;; Homepage: https://github.com/tgbugs/orgstrap
;; Version: 9999
;; Package-Requires: ((emacs "27.1"))
;; Is-Version-Of: https://raw.githubusercontent.com/tgbugs/orgstrap/master/ow.el
;; Reval-Get-Immutable: ow--reval-update

;;;; License and Commentary

;; License:
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ow.el is the catch-all file that includes all the functionality
;; that you might want to use in an orgstrap block in a single place
;; including functions that are also packaged independently such as
;; the reval-* and securl-* functionality. The normal way to use it
;; is to use `reval-minimal' to obtain all the functionality, and then
;; use `reval-reload-latest' to cache a persistent copy and reload from
;; that file so that all xrefs can be resolved.

;; ow.el is compatible with `reval-update'.

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

(defvar-local defl--local-defuns nil
  "A hash table that maps global closures to local function symbols.
Needed to dispatch on command passed to :around advice.")

(defvar-local defl--local-defun-names nil
  "A hash table that maps global function symbols to local function symbols.")

(defun defl--has-local-defuns (command &rest args)
  "Advise COMMAND with ARGS to check if there are buffer local defuns."
  (let ((command (or (and defl--local-defuns
                          (gethash command defl--local-defuns))
                     command)))
    (apply command args)))

(defmacro defl (name arglist &optional docstring &rest body)
  "Define a buffer local function.
ARGLIST, DOCSTRING, and BODY are passed unmodified to `defun'

WARNING: If you redefine NAME with `defun' after using `defun-local'
then the current implementation will break."
  (declare (doc-string 3) (indent 2))
  (unless defl--local-defuns
    (setq-local defl--local-defuns (make-hash-table :test #'equal)))
  (unless defl--local-defun-names
    (setq-local defl--local-defun-names (make-hash-table)))
  (let ((docstring (if docstring (list docstring) docstring))
        (local-name (or (gethash name defl--local-defun-names)
                        (puthash name (cl-gentemp (format "%S-" name)) defl--local-defun-names))))
    `(prog1
         (defun ,local-name ,arglist ,@docstring ,@body)
       (unless (fboundp ',name)
         (defun ,name (&rest args) (error "global stub for defun-local %s" #',name))
         (put ',name 'defun-local-stub t))
       (puthash (symbol-function #',name) #',local-name defl--local-defuns) ; XXX broken if the stub is overwritten
       (advice-add #',name :around #'defl--has-local-defuns))))

(defalias 'defun-local #'defl)

(defun defl-defalias-local (symbol definition &optional docstring)
  "Define a buffer local alias. NOTE only works for functions.
It is not really needed for variables since `setq-local' covers
nearly every use case. Note that the way this is defined uses
`defun-local' so it probably does not behave like a real alias."
  (if (symbol-function definition)
      (defun-local symbol (&rest args)
        docstring
        (apply definition args))
    (error "%S does not point to a function" definition)))

(defun defl--raw-symbol-function (name)
  "Return unadvised form of NAME. NOT THREAD SAFE."
  (if (advice-member-p #'defl--has-local-defuns name)
      (unwind-protect
          (progn
            (advice-remove name #'defl--has-local-defuns)
            (symbol-function name))
        ;; FIXME > assuming that name was previously advised
        (advice-add name :around #'defl--has-local-defuns))
    (symbol-function name)))

(defun defl--fmakunbound-local (command &rest args)
  "Advise COMMAND `fmakunbound' to be aware of `defun-local' forms."
  (if defl--local-defun-names
      (let* ((name (car args))
             (local-name (gethash name defl--local-defun-names)))
        ;; FIXME If we mimic the behavior of defvar-local then
        ;; we should never remove the error stub, but this is
        ;; a bit different because we can't change how defun works to
        ;; mimic how setq works and then have defun-default that mimics
        ;; how setq-default works, the behavior of local variables is
        ;; already confusing enough without having to also deal with the
        ;; the fact that defun and defvar have radically different behavior
        ;; with regard to redefinition
        ;; FIXME it would still be nice to be able to remove the advice
        ;; from the global function when the last local function ceases
        ;; to be defined but that can be for later
        (if local-name
            (progn
              (apply command (list local-name))
              (remhash (defl--raw-symbol-function name) defl--local-defuns)
              (remhash name defl--local-defun-names))
          (apply command args)))
    (apply command args)))

;;(advice-add #'fmakunbound :around #'defl--fmakunbound-local)

(defun ow-run-command (command &rest args)
  "Run COMMAND with ARGS. Raise an error if the return code is not zero."
  ;; I'm being a good namespace citizen and prefixing this with ow- but
  ;; 99% of the time I'm going to have (defalias 'run-command #'ow-run-command)
  ;; in my orgstrap blocks that reval ow.el (since ow-min.el has it already)
  ;;(message "%S" (mapconcat #'identity (cons command args) " "))
  (with-temp-buffer
    (let* ((return-code (apply #'call-process command nil (current-buffer) nil args))
           (string (buffer-string)))
      (if (not (= 0 return-code))
          (error "code: %s stdout: %S" return-code string)
        string))))

(defvar securl-default-cypher 'sha256)  ; remember kids, always publish the cypher with the checksum

(defun securl-path-checksum (path &optional cypher)
  "not as fast as using sha256sum, but requires no dependencies
1.4s vs .25s for ~60mb"
  (let ((cypher (or cypher securl-default-cypher)))
    (with-temp-buffer
      (insert-file-contents-literally path)
      (secure-hash cypher (current-buffer)))))

(defun securl (cypher checksum url path)
  "Securely fetch URL to PATH only if it matches CHECKSUM under CYPHER.
Files that do not match the checksum are quarantined."
  ;; unless the file exists or the checksum matches fetch and check
  (unless (and (file-exists-p path)
               (let ((existing-checksum (securl-path-checksum path cypher)))
                 (or (string= checksum existing-checksum)
                     ;; (not (message "checksum mismatch:\n%S\n%S" checksum existing-checksum))
                     (not (rename-file path
                                       (make-temp-file (concat path "._existing_failure_."))
                                       t)))))
    (let ((path-temp (make-temp-file (concat path "._fetching_."))))
      (url-copy-file url path-temp t)
      (let ((new-checksum (securl-path-checksum path-temp cypher)))
        (if (string= checksum new-checksum)
            (rename-file path-temp path)
          (let ((path-failure (make-temp-file (concat path "._checksum_failure_."))))
            (rename-file path-temp path-failure t)
            (error "checksum FAILED for path! %s" path-failure))))))
  ;; return nil in all cases the calling scope has the path and
  ;; whatever is at that path must have passed the current checksum
  nil)

(defun ow-url-head-ok (url)
  "Check if URL is up and OK using HTTP HEAD.
All errors are silenced."
  (let ((url-request-method "HEAD"))
    (condition-case nil
        (with-current-buffer (url-retrieve-synchronously url)
          ;;(buffer-substring (point-min) url-http-end-of-headers)
          (goto-char 0)
          (re-search-forward "^HTTP.+OK$"))
      (error nil))))

(defvar ow-package-archives '(("melpa" . "https://melpa.org/packages/")
                              ("org" . "https://orgmode.org/elpa/")))

(defun ow-enable-use-package ()
  "Do all the setup needed `use-package'.
This needs to be called with (eval-when-compile ...) to the top level prior
to any use of `use-package' otherwise it will be missing and fail"
  ;; package archives is not an argument to this function to ensure that
  ;; there is only one place that needs to be updated if an archive location
  ;; changes, namely this library, updating that is easier to get right using
  ;; the `reval-update' machinery
  (require 'package)
  (dolist (pair ow-package-archives) (add-to-list 'package-archives pair t))
  (unless package--initialized
    (package-initialize))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

(defmacro ow-use-packages (&rest names)
  "enable multiple calls to `use-package' during bootstrap"
  (cons 'progn (mapcar (lambda (name) `(use-package ,name)) names)))

;; mouse behavior

(defun ow--safe-cycle (event &optional promote-to-region)
  "Bind this to mouse-1 for sane clickable cycling behavior."
  (interactive "e\np")
  (let ((face (get-char-property (point) 'face)))
    (unless (and face (listp face) (memq 'org-block face))
      (unwind-protect
          (progn
            (remove-hook 'org-cycle-hook #'org-optimize-window-after-visibility-change t)
            (org-cycle))
        (add-hook 'org-cycle-hook #'org-optimize-window-after-visibility-change nil t))))
  ;; have to chain `mouse-set-point' otherwise double click to highlight words etc. fails
  (mouse-set-point event promote-to-region))

(defun ow-enable-mouse-cycle ()
  "Allow left mouse to cycle org headings."
  (interactive)
  ;; reset `org-cycle-hook' as a local variable so that
  ;; we can add/remove individual hooks without messing
  ;; with the global behavior which might some day not
  ;; be purely single threaded (heh)
  (setq-local org-cycle-hook org-cycle-hook)
  (local-unset-key [mouse-1])
  (local-set-key [mouse-1] #'ow--safe-cycle))

(defun ow-recenter-on-mouse ()
  "Recenter the cursor line so that it is under the mouse."
  ;; after much digging using `mouse-pixel-position' together
  ;; with `pos-at-x-y' seems to be what we want, `mouse-position'
  ;; and `window-edges' are decidedly NOT the right solution
  ;; `pos-at-x-y' is able to call into the C code to get something
  ;; much closer to what is produced by an actual mouse event
  ;; https://emacs.stackexchange.com/questions/30852 has the wrong solution
  (interactive)
  (let* ((mpp (mouse-pixel-position))
         (position-list (posn-at-x-y (cadr mpp)
                                     (cddr mpp)
                                     (selected-frame)
                                     nil))
         ;;(asdf (message "%s" hrm))
         (mouse-line (cdr (posn-actual-col-row position-list)))
         (cursor-line (- (line-number-at-pos)
                         (line-number-at-pos (window-start))))
         (offset (- mouse-line cursor-line)))
    ;;(message "ml: %s cl: %s offset: %s" mouse-line cursor-line offset)
    (scroll-down offset)))

;; random useful functions

(defun ow-fold-headline (&optional name)
  "Set visibility property of headline with NAME or previous visible to folded."
  ;; https://orgmode.org/manual/Using-the-Property-API.html
  (save-excursion
    (if name
        (goto-char (org-find-exact-headline-in-buffer name))
      (org-previous-visible-heading 0))
    (org-entry-put nil "visibility" "folded")
    (save-buffer)))

;; don't export buttons

(defun ow-link-no-export (path desc format)
  "Return nothing for export" ; FIXME broken ???
  "")

(defun ow-button (link-name function)
  "Create a new button type."
  (org-link-set-parameters link-name :export #'ow-link-no-export :follow function))

(defmacro ow-defbutton (link-name &rest body)
  `(ow-button ,link-name (lambda () ,@body)))

;; TODO defalias defbutton ow-defbutton

;; hide headline for future startups

(org-link-set-parameters "FOLD-HEADLINE" :export #'ow-link-no-export :follow
                         (lambda (&optional nothing)
                           (ow-fold-headline)))

;; run the next #+begin_src block

(org-link-set-parameters "RUN" :export #'ow-link-no-export :follow
                         (lambda (&optional nothing)
                           (org-next-block nil)
                           (org-babel-execute-src-block)))

;; run the previous src block (doesn't work if there are results)

(org-link-set-parameters "RUNPREV" :export #'ow-link-no-export :follow
                         (lambda (&optional nothing)
                           (org-previous-block nil)
                           (org-babel-execute-src-block)))

;; run the next #+call: TODO we should be able to do this with mouse-1?

(org-link-set-parameters "RUNC" :export #'ow-link-no-export :follow
                         (lambda (&optional nothing)
                           (save-excursion
                             (re-search-forward "#\\+call:")
                             (org-ctrl-c-ctrl-c))))

;; adjust font size for the current buffer

(org-link-set-parameters "TEXT-LARGER" :export #'orsgrap--nox :follow
                         (lambda (&optional nothing)
                           (text-scale-adjust 1)
                           (ow-recenter-on-mouse)))

(org-link-set-parameters "TEXT-SMALLER" :export #'ow-link-no-export :follow
                         (lambda (&optional nothing)
                           (text-scale-adjust -1)
                           (ow-recenter-on-mouse)))

(org-link-set-parameters "TEXT-RESET" :export #'ow-link-no-export :follow
                         (lambda (&optional nothing)
                           (text-scale-adjust 0)
                           (ow-recenter-on-mouse)))

(defun ow--headline-faces ()
  "Set face for all headline levels to be bold."
  (mapcar (lambda (n) (set-face-attribute (intern (format "org-level-%s" n)) nil :bold t))
          (number-sequence 1 8)))

(defun ow-enable-config-familiar-1 ()
  "Minimal config to achieve something more familiar for non-Emacs users.

Uses `cua-mode' with additional tweak for undo bindings.
NOTE: `undo-fu' is required for Emacs < 28."

  ;; Enable familiar copy/paste keybindings
  (cua-mode t)
  ;; enable Cmd Shift Z for apple users Ctrl y for windows
  (when (fboundp #'undo-redo)
    (if (eq system-type 'darwin)
        (local-set-key "C-Z" #'undo-redo)
      (local-set-key "C-y" #'undo-redo)))

  ;; Use left mouse to cycle
  (ow-enable-mouse-cycle)

  ;; Mouse wheel behavior
  (setq-local mouse-wheel-progressive-speed nil)
  (setq-local mouse-wheel-scroll-amount '(3 ((shift) . hscroll)))

  ;; Mouse on scroll bar behavior
  ;; TODO this is not quite right
  (global-unset-key [vertical-scroll-bar mouse-1])
  (global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)

  ;; default shift functionality is usually not needed in ow files and
  ;; the message when you try to use it can be extremely confusing
  (setq-local org-support-shift-select t)

  ;; Enable tab-bar-mode
  (tab-bar-mode t)

  ;; Use the whiteboard theme
  (load-theme 'whiteboard)

  ;; Set headline faces, sligh improvements to the whiteboard defaults
  (ow--headline-faces)
  (set-face-attribute 'org-block-begin-line nil :extend t :foreground "black" :background "silver")
  (set-face-attribute 'org-block-end-line nil :extend t :foreground "black" :background "silver")
  (set-face-attribute 'org-block nil :extend t :background "white"))

(defun ow--reval-update ()
  "Get the immutable url for the current remote version of this file."
  (reval-get-imm-github "tgbugs" "orgstrap" "ow.el"))

(provide 'ow)

;;; ow.el ends here
