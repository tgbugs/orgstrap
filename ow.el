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
;; the reval-* and securl-* functionality.  The normal way to use it
;; is to use `reval-minimal' to obtain all the functionality, and then
;; use `reval-reload-latest' to cache a persistent copy and reload from
;; that file so that all xrefs can be resolved.

;; ow.el is compatible with `reval-update'.

;;; Code:

(unless (featurep 'reval)
  (require 'cl-lib)

  (defgroup reval nil
    "Minimal remote evaluation of elisp code."
    :tag "reval"
    :group 'applications)

  (defcustom reval-default-cypher 'sha256
    "Default cypher to use to fill in a hole in `reval'."
    :type 'symbol
    :options (if (fboundp #'secure-hash-algorithms) (secure-hash-algorithms) '(sha256))
    :group 'reval)

  (defvar reval-cache-directory (concat user-emacs-directory "reval/cache/")
    "The directory where retrieved .el files are saved.")

  (defvar reval-failed-buffer-list nil "List of failed reval buffers.") ; XXX FIXME this is dumb
  ;; use the buffer to track the state using `reval-fail' because then
  ;; the process composes with the other tools we have for interacting
  ;; with revaled files and caches etc.

  (defvar reval-evaled-buffer-list nil "List of evaled reval buffers.")

  (defvar-local reval--pending-updates nil
    "Internal variable use to coordinate updates for a single buffer.")

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
      (save-excursion
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
                (error "Audit failed.  Checksum will not be calculated for %s"
                       (buffer-file-name (current-buffer)))))

            ;; need to ensure that file is actually elisp
            ;; note that in some cases read can succeed
            ;; even when a file is not elisp e.g. an html
            ;; file can sometimes read without error but
            ;; will fail on eval

            ;; elisp check by major mode
            (unless (eq major-mode 'emacs-lisp-mode)
              (error "Not an Emacs Lisp file!"))

            ;; elisp check by read
            (condition-case nil
                (read (concat "(progn\n"
                              (buffer-substring-no-properties (point-min) (point-max))
                              "\n)"))
              (error (error "Not an Emacs Lisp file!")))

            ;; return the checksum
            (intern (secure-hash cypher (current-buffer))))))))

  (defun reval-resum-minimal (cypher buffer)
    "Checksum of BUFFER under CYPHER." ; minimal for maximal porability
    ;; not used since the expression takes up less space
    (intern (secure-hash cypher buffer)))

  (defalias 'reval-resum #'reval-resum-review)

  (defvar reval--make-audit t "Dynamic variable to control audit during `reval--make'.")
  ;; the control of audit behavior is intentionally excluded from the
  ;; arguments of `reval--make' so that top level calls must audit
  (defun reval--make (cypher path-or-url)
    "Make a `reval' expression from CYPHER and PATH-OR-URL.
  This should not be used directly at the top level see docs for `reval'
  for a better workflow."
    (unless reval--make-audit
      (warn "`reval--make' not auditing %S" path-or-url))
    (let ((checksum (reval-resum-review cypher (reval-id->buffer path-or-url) reval--make-audit)))
      `(reval ',cypher ',checksum ,path-or-url)))

  (defun reval-audit (&optional universal-argument)
    "Audit the reval under the cursor." ; FIXME this needs a LOT of work
    (interactive)
    (cl-multiple-value-bind (cypher checksum path-or-url _alternates _b _e) ; FIXME probably loop here
        (reval--form-at-point)
      (let* ((buffer (with-url-handler-mode
                       (find-file-noselect path-or-url)))
             (buffer-checksum (reval-resum cypher buffer t)))
        (eq buffer-checksum checksum))))

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

  (defun reval-cache-path (checksum &optional basename)
    "Return the path to the local cache for a given CHECKSUM.
  If BASENAME is provided a wildcard is not used.  This is mostly
  to make the calls more human readable for debugging but also
  makes it easier to catch cases where the wrong checksum was passed."
    (let* ((name (symbol-name checksum))
           (subdir (substring name 0 2))
           (cache-path (concat reval-cache-directory subdir "/" name "-" (or basename "*"))))
      (if basename
          cache-path
        (let ((expanded (file-expand-wildcards cache-path)))
          (if expanded
              ;; I guess a strict rename could hit a dupe but hitting a
              ;; hash collision here would be ... astronimical odds
              (car expanded)
            nil)))))

  (defun reval--write-cache (buffer cache-path)
    "Write BUFFER to CACHE-PATH.  Create the parent if it doesn not exist."
    (let ((parent-path (file-name-directory cache-path))
          make-backup-files)
      (unless (file-directory-p parent-path)
        (make-directory parent-path t))
      (with-current-buffer buffer
        (write-file cache-path))))

  (defun reval-find-cache (&optional universal-argument)
    "Jump to the cache for a given reval call.
  At the moment UNIVERSAL-ARGUMENT is a placeholder."
    (interactive)
    (cl-multiple-value-bind (_cypher checksum path-or-url _alternates _b _e)
        (reval--form-at-point)
      (let ((cache-path (reval-cache-path checksum)))
        (if (file-exists-p cache-path)
            (let ((buffer (find-file-noselect cache-path)))
              (with-current-buffer buffer (emacs-lisp-mode))
              (pop-to-buffer-same-window buffer))
          (error "No cache for %s" path-or-url)))))

  (defun reval-fail (&rest args)
    "Embed in buffer if audit fails on ARGS so that there is a record."
    (error "reval audit failed for: %S" args))

  (defun reval (cypher checksum path-or-url &rest alternates)
    "Open PATH-OR-URL, match CHECKSUM under CYPHER, then eval.
  If an error is encountered try ALTERNATES in order.

  The simplest way to populate a `reval' expression starting from just
  PATH-OR-URL is to write out expression with CYPHER and CHECKSUM as a
  nonsense values.  For example (reval ? ? \"path/to/file.el\").  Then
  run \\[reval-update-simple] (M-x `reval-update-simple') to populate
  CYPHER and CHECKSUM."
    (reval--get-buffer cypher checksum path-or-url alternates #'eval-buffer))

  (defun reval--get-buffer (cypher checksum path-or-url &optional alternates do-with-buffer)
    "generic implementation that gets a buffer and can run a function
  DO-WITH-BUFFER in that buffer to enforce invariants, eval the buffer, etc.
  Note that this function ALWAYS returns the buffer, so DO-WITH-BUFFER should only
  be used to trigger a failure mode before the buffer is retruned, not used to get
  a return value from the buffer."
    (let (found-buffer (cache-path (reval-cache-path checksum (file-name-nondirectory path-or-url))))
      (with-url-handler-mode
        (cl-loop for path-or-url in (cons cache-path (cons path-or-url alternates))
                 do (if (file-exists-p path-or-url)
                        (let* ((buffer (reval-id->buffer path-or-url))
                               (_ (when (string= path-or-url cache-path)
                                    (with-current-buffer buffer (emacs-lisp-mode))))
                               ;; FIXME this is still not right ... can error due to not elisp
                               (buffer-checksum (reval-resum cypher buffer)))
                          (if (eq buffer-checksum checksum)
                              (let ((buffer
                                     (if (string= path-or-url cache-path)
                                         buffer
                                       ;; save to cache and switch buffer before eval for xrefs
                                       (reval--write-cache buffer cache-path)
                                       (find-file-noselect cache-path))))
                                (when do-with-buffer
                                  (with-current-buffer buffer
                                    (funcall do-with-buffer)))
                                (setq found-buffer buffer))
                            (reval--add-buffer-to-list buffer 'reval-failed-buffer-list)
                            (funcall (if alternates #'warn #'error)
                                     ;; if alternates warn to prevent an early failure
                                     ;; from blocking later potential successes otherwise
                                     ;; signal an error
                                     "reval: checksum mismatch! %s" path-or-url)))
                      (warn "reval: file does not exist! %s" path-or-url))
                 until found-buffer))
      (unless found-buffer
        (error "reval: all paths failed!"))
      found-buffer))

  (defun reval-view-failed ()
    "View top of failed reval buffer stack and kill or keep."
    (interactive)
    (when reval-failed-buffer-list
      (save-window-excursion
        (with-current-buffer (car reval-failed-buffer-list)
          (switch-to-buffer (current-buffer))
          (when (y-or-n-p "Kill buffer? ")
            (kill-buffer))))))

  ;;; machinery to get the latest immutable url for a revaled file

  (require 'lisp-mnt)

  (defvar url-http-end-of-headers)
  (defun reval-url->json (url)  ; see utils.el
    "Given a URL string return json as a hash table."
    (json-parse-string
     (with-current-buffer (url-retrieve-synchronously url)
       (buffer-substring url-http-end-of-headers (point-max)))))

  (defun reval--get-new-immutable-url ()
    "Get the immutable url for the current buffer."
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
    "Get the immutable url for PATH on BRANCH in a github remote for REPO at GROUP."
    (let* ((branch (or branch "master"))
           (branch-url
            (format "https://api.github.com/repos/%s/%s/git/refs/heads/%s"
                    group repo branch))
           (branch-sha (gethash "sha" (gethash "object" (reval-url->json branch-url))))
           (url
            (format "https://api.github.com/repos/%s/%s/commits?path=%s&page=1&per_page=1&sha=%s"
                    group repo path branch-sha))
           (result (reval-url->json url))
           (sha (gethash "sha" (elt (reval-url->json url) 0))))
      (format "https://raw.githubusercontent.com/%s/%s/%s/%s" group repo sha path)))

  (defun reval-header-is-version-of (&optional file)
    "Return the Is-Version-Of: header for FILE or current buffer."
    ;; this was originally called Latest-Version but matching the
    ;; datacite relationships seems to make more sense here esp.
    ;; since this is literally the example from the documentation
    (lm-with-file file
      (lm-header "is-version-of")))

  (defun reval-header-get-immutable (&optional file)
    "Return the Reval-Get-Immutable: header for FILE or current buffer.

  The value of this header should name a function in the current
  file that returns an immutable name that points to the current
  remote version of the the current file.

  The implementation of the function may assume that the reval
  package is present on the system."
    ;; there will always have to be a function because even if the
    ;; remote does all the work for us we will still have to ask the
    ;; remote to actually do the dereference operation, since we can't
    ;; gurantee that all remotes even have endpoints that behave this
    ;; functionality we can't implement this once in reval, so each
    ;; file implements this pattern itself, or worst case, the
    ;; update function can be supplied at update time if there is a
    ;; useful remote file that doesn't know that it is being revaled
    (lm-with-file file
      (lm-header "reval-get-immutable")))

  ;;; internals for `reval-sync' workflow

  (defun reval--audit-change (buffer-old url-new buffer-source cypher begin)
    "Audit the changes made in URL-NEW relative to BUFFER-OLD.
  If the audit passes, a checksum is calculated under CYPHER for
  the new buffer, and a new reval form is inserted into BUFFER-SOURCE
  starting at position BEGIN which corresponds to the beginning of the
  reval form that was the source for BUFFER-OLD."
    (let* ((buffer-new (with-url-handler-mode (find-file-noselect url-new)))
           (buffer-review (reval--diff buffer-old buffer-new)))
      ;; audit
      ;; TODO see if we need something more than this, for small diffs it seems
      ;; to work fairly well, it captures the core functionality needed, and
      ;; if users are curious about the changes, they can accept or reject,
      ;;; and then look up the changes in git without complicating this workflow
      (let*
          ((ok
            (save-excursion
              (save-window-excursion
                (with-current-buffer buffer-review
                  (display-buffer buffer-review)
                  ;; not using `y-or-n-p' since it is too easy
                  (let ((yes (yes-or-no-p "Audit of changes ok? ")))
                    (unless yes
                      (warn "Audit failed.  Checksum will not be calculated for %s"
                            (buffer-file-name buffer-new)))
                    yes)))))
           (checksum-new (if (not ok) 'audit-failed (intern (secure-hash cypher buffer-new))))
           (to-insert
            (prin1-to-string
             `(,(if (not ok) 'reval-fail 'reval) ',cypher ',checksum-new ,url-new 'NEW))))
        (with-current-buffer buffer-source
          (save-excursion
            (goto-char begin)
            (insert to-insert "\n")))
        ok)))

  (defun reval--diff (buffer-old buffer-new)
    "Construct a buffer that is the diff between BUFFER-OLD and BUFFER-NEW."
    ;;(ediff-buffers buffer-old buffer-new)
    ;; lol no-select noselect inconsistency
    (diff-no-select buffer-old buffer-new nil nil))

  (defun reval--get-changed ()
    "Collect thunks to update the reval expressions in the current buffer that have new versions."
    (save-excursion
      (goto-char (point-min))
      (let (out (buffer-source (current-buffer)))
        (while (re-search-forward "(reval[[:space:]]" nil t)
          (cl-multiple-value-bind (cypher checksum path-or-url-raw alternates begin)
              (reval--form-at-point)
            (let* ((path-or-url
                    (or (and (stringp path-or-url-raw) path-or-url-raw) ; XXX why does stringp return t !??!?
                        ;; FIXME DANGERZONE !? (yes very)
                        (eval path-or-url-raw)))
                   (buffer
                    (reval--get-buffer
                     cypher checksum path-or-url alternates
                     #'reval--eval-buffer-when-not-fboundp))
                   (url-new (with-current-buffer buffer (reval--get-new-immutable-url))))
              (unless (string= url-new path-or-url)
                ;; FIXME TODO local urls should check for version control so that
                ;; checksums can be updated before pushing, but that is more involved
                (setq
                 out
                 (cons
                  (list
                   (reval--make-diff-thunk buffer url-new buffer-source cypher begin)
                   alternates)
                  out)))))
          (forward-sexp))
        out)))

  (defun reval--eval-buffer-when-not-fboundp ()
    "Run inside `reval--get-buffer' to avoid revaling the buffer if
  the imm url function if it is already fboundp"
    (let ((get-imm-name (reval-header-get-immutable)))
      (if get-imm-name
          (let ((get-imm (intern get-imm-name)))
            (unless (fboundp get-imm)
              (eval-buffer))))))

  (defun reval--make-diff-thunk (buffer-old url-new buffer-source cypher begin)
    "Create the thunk that kicks off the update workflow."
    (lambda () (reval--audit-change buffer-old url-new buffer-source cypher begin)))

  (defun reval--dquote-symbolp (thing)
    "Match pattern ''THING.
  Useful when dealing with quoted symbols in the outpub or a `read'.
  For example elt 2 of '(reval 'sha256 ? \"file.el\")."
    (and (consp thing)
         (eq (car thing) 'quote)
         (consp (cdr thing))
         (symbolp (cadr thing))))

  (defun reval--form-at-point ()
    "Extract the components of the reval expression at the current point."
    (save-excursion
      (re-search-forward " ")
      (re-search-backward "(reval[[:space:]]")
      (let ((begin (point)))
        (forward-sexp)
        (let ((raw (read (buffer-substring-no-properties begin (point))))
              (end (point)))
          ;;(message "aaaaa: %S %S %S" raw (symbolp (elt raw 1)) (type-of (elt raw 1)))
          (let ((cypher (let ((cy (elt raw 1)))
                          ;; '(sigh 'sigh) XXX the usual eval dangerzone >_<
                          (if (reval--dquote-symbolp cy) (eval cy) reval-default-cypher)))
                (checksum (let ((cs (elt raw 2)))
                            (if (reval--dquote-symbolp cs) (eval cs) nil)))
                (path-or-url (elt raw 3))
                (alternates (cddddr raw)))
            (cl-values cypher checksum path-or-url alternates begin end))))))

  (defun reval-check-for-updates (&optional universal-argument) ; TODO reval-sync ?
    "Check current buffer revals for updates.
  UNIVERSAL-ARGUMENT is a placeholder."
    (interactive)
    ;; search and collect all calls to reval in the current buffer? all org files? ???
    ;; open the current reval in a buffer
    ;; get the package info if avaiable
    ;; warn about all revals that cannot be updated due to missing metadata?
    (let* ((pending-updates (reval--get-changed))
           (count (length pending-updates))
           (updates? (if (> count 1) "updates" "update")))
      (setq-local reval--pending-updates pending-updates)
      ;; TODO display a message about pending updates or a buffer
      ;; with the pending updates? probably easier to do the latter
      ;; and have approve/deny buttons or something
      (if (> count 0)
          (message "%s %s found for %s run `reval-do-updates' to audit changes."
                   count updates? (buffer-file-name))
        (message "No reval updates found for %s" (buffer-file-name)))))

  (defun reval-do-updates (&optional universal-argument)
    "Audit and insert pending updates.
  UNIVERSAL-ARGUMENT is a placeholder."
    ;; XXX `reval-update' isn't here because it would be for a single
    ;; form, but our workflow doesn't do that right now
    (interactive)
    (while reval--pending-updates
      (let* ((update (pop reval--pending-updates))
             (do-update (car update))
             (success (funcall do-update))))))

  (defun reval-form-checksum-at-point (&optional universal-argument)
    "Get the checksum in the reval form at point.  UNIVERSAL-ARGUMENT is a placeholder."
    (interactive)
    (cl-multiple-value-bind (_cypher checksum _path-or-url _alternates _b _e)
        (reval--form-at-point)
      checksum))

  ;; user facing functionality

  (defun reval-update-simple (&optional universal-argument)
    "Update the checksum for the reval sexp under the cursor or up the page.
  Useful when developing against a local path or a mutable remote id.
  If UNIVERSAL-ARGUMENT is non-nil then `reval-audit' is skipped, please use
  this functionality responsibly."
    (interactive "P")
    (with-url-handler-mode
      (let ((reval--make-audit (not universal-argument))
            (sigh (point)))
        (cl-multiple-value-bind (cypher checksum path-or-url alternates begin end)
            (reval--form-at-point)
          (unless (memq cypher (secure-hash-algorithms))
            (error "%S is not a known member of `secure-hash-algorithms'" cypher))
          (let ((new (reval--make cypher path-or-url))
                (print-quoted t)
                print-length
                print-level)
            (delete-region begin end)
            (insert (prin1-to-string
                     (if alternates ; don't cons the old checksum, repeated invocations grow
                         (append new (cons ''OLD> alternates))
                       new))))
          (goto-char sigh)))))

  (defun reval-sync (&optional universal-argument)
    "Check for, audit, and insert updates for the current buffer.
  UNIVERSAL-ARGUMENT is a placeholder."
    (interactive)
    (reval-check-for-updates)
    (when reval--pending-updates
      (reval-do-updates)))

  )

(unless (featurep 'defl)
  (require 'cl-lib)

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
           (defun ,name (&rest args) (error "Global stub for defun-local %s" #',name))
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
  )

(defvar ow-forward-subprocess-streams t "Forward streams from subprocesses.")

(defun ow--forward-stream (buffer point printcharfun)
  "Use `princ' on BUFFER after POINT using PRINCHARFUN to output.
Helper function to forward stderr and stdout from subprocesses."
  ;; tried to add a prefix to clarify out/err distinctions but it
  ;; turns out that even the simplest test cases show that that is a
  ;; futile attempt ... thanks unix
  (with-current-buffer buffer
    (let ((new-point (point)))
      (unless (= new-point point)
        (princ (buffer-substring point new-point) printcharfun))
      new-point)))

(defun ow--my-balls (process princharfun &optional no-forward)
  (let* ((buffer (process-buffer process))
         (point (with-current-buffer buffer (point))))
    (while (accept-process-output process)
      (when (and noninteractive (not no-forward))
        (let (message-log-max)
          (setq point (ow--forward-stream buffer point princharfun)))))
    ;; we don't actually want to print the junk in the buffer that
    ;; shows up after the process exits, unfortunately the behavior is
    ;; inconsistent and sometimes the junk prints and sometimes it
    ;; does not depending on the exact interaction between the threads
    ;; and the process, so we call this one last time to get everything
    (ow--forward-stream buffer point princharfun)
    nil))

(defun ow--accept-and-forward-process-output (process stderr-process &optional no-forward)
  "Accept output from PROCESS and STDERR-PROCESS when `noninteractive' forward streams.

WARNING: does not gurantee sequencing of stdout and stderr events.

If the optional NO-FORWARD argument is non-nil do not forward.

An alternate approach might be to use `set-process-filter'."

  ;; https://github.com/tkf/emacs-request/issues/203
  (let ((tout (make-thread (lambda () (ow--my-balls process standard-output no-forward))))
        (terr (make-thread (lambda () (ow--my-balls stderr-process #'external-debugging-output no-forward)))))
    ;; we have to bind each process to the new threads so that they
    ;; can be read by each thread, in this way we can nearly get
    ;; behavior that matches wait for multiple objects

    ;; XXX WARNING: even with this behavior the ordering of events
    ;; IS NOT PRESERVED, see the stochastic behavior of the err-out
    ;; alternating chain test, clearly shows a race condition
    (set-process-thread process tout)
    (set-process-thread stderr-process terr)
    (thread-join tout)
    (thread-join terr)))

(defun ow--aafpo-<-26 (process stderr-process &optional no-forward)
  "Variant of `ow--accept-and-forward-process-output' that works before Emacs 26."
  (let ((stdout-buffer (process-buffer process))
        (stderr-buffer (process-buffer stderr-process))
        (stdout-point 1)
        (stderr-point 1))
    (cl-loop
     for p in (list process stderr-process) do
     (while (accept-process-output p)
       (when (and noninteractive (not no-forward))
         (let (message-log-max)
           (setq stdout-point (ow--forward-stream stdout-buffer stdout-point standard-output))
           (setq stderr-point (ow--forward-stream stderr-buffer stderr-point #'external-debugging-output))))))
    (ow--forward-stream stdout-buffer stdout-point standard-output)
    (ow--forward-stream stderr-buffer stderr-point #'external-debugging-output)))

(when (< emacs-major-version 26)
  (defalias 'ow--accept-and-forward-process-output #'ow--aafpo-<-26))

(defun ow-run-command (command &rest args)
  "Run COMMAND with ARGS.
Raise an error if the return code is not zero."
  ;; TODO maybe implement this in terms of ow-run-command-async ?
  ;; usually (defalias 'run-command #'ow-run-command)
  (let ((stdout-buffer (generate-new-buffer " rc stdout"))
        (stderr-buffer (generate-new-buffer " rc stderr")))
    (unwind-protect
        (let ((process
               (make-process
                :name (concat "run-command: " command)
                :buffer stdout-buffer
                :stderr stderr-buffer
                :command (cons command args))))
          (ow--accept-and-forward-process-output
           process
           (get-buffer-process stderr-buffer)
           (not ow-forward-subprocess-streams))
          (let ((ex (process-exit-status process)))
            (if (= 0 ex)
                (with-current-buffer stdout-buffer (buffer-string))
              (error "Command %s failed code: %s stdout: %S stderr: %S"
                     command ex
                     (with-current-buffer stdout-buffer (buffer-string))
                     (with-current-buffer stderr-buffer (buffer-string))))))
      (cl-loop ; workaround for bug#56002
       for buffer in (list stdout-buffer stderr-buffer) do
       (let ((p (get-buffer-process buffer)))
         (when p
           (set-process-query-on-exit-flag p nil))))
      (kill-buffer stdout-buffer)
      (kill-buffer stderr-buffer))))

(defun ow-run-command-24 (command &rest args)
  "Run COMMAND with ARGS. Raise an error if the return code is not zero.
This is retained for compatibility with Emacs 24 since `make-process' was
introduced in version 25."
  (with-temp-buffer
    (let* ((return-code (apply #'call-process command nil (current-buffer) nil args))
           (string (buffer-string)))
      (if (not (= 0 return-code))
          (error "Command %s failed code: %s stdout: %S" command return-code string)
        string))))

(when (< emacs-major-version 25)
  (defalias 'ow-run-command #'ow-run-command-24))

(defun ow--default-sentinel (process message &optional stderr-process)
  "An example sentinel for async processes.
PROCESS is the process that changed status and MESSAGE is the
message related to that change.  The STDERR-PROCESS is passed as
an optional argument if :stderr was set (which it always is when
using `ow-run-command-async')."
  (message "%s %s %s"
           message
           (process-status process)
           (and stderr-process (process-status stderr-process)))
  (message "stdout: %S stderr: %S"
           (with-current-buffer (process-buffer process) (buffer-string))
           (and stderr-process (with-current-buffer (process-buffer stderr-process) (buffer-string)))))

(cl-defun ow-run-command-async (command &rest args &key sentinel &allow-other-keys)
  "Run COMMAND with ARGS asynchronously.

SENTINEL is a function that has two required arguments, and MUST
ACCEPT AN ADDITIONAL OPTIONAL ARGUMENT for stderr-process. This
allows the sentinel process to be use as a normal sentinel
function as well.

Reminder that kwargs must come before rest when calling a cl-defun."
  (let* ((args (or (and (memq :sentinel args)
                        (cl-remove-if (lambda (x) (or (not x) (eq x :sentinel)))
                                      (plist-put args :sentinel nil)))
                   args))
         (stdout-buffer (generate-new-buffer (concat " process-buffer-" command)))
         (stderr-buffer (generate-new-buffer (concat " process-buffer-stderr" command)))
         (stderr-process
          (make-pipe-process
           :name (concat "process-stderr-" command)
           :buffer stderr-buffer))
         (wrapped-sentinel
          (if sentinel
              (lambda (process message)
                (unwind-protect
                    (funcall sentinel process message stderr-process)
                  (when (memq (process-status process) '(exit signal))
                    (kill-buffer stdout-buffer)
                    (kill-buffer stderr-buffer))))
            (lambda (process message)
              (when (memq (process-status process) '(exit signal))
                (kill-buffer stdout-buffer)
                (kill-buffer stderr-buffer)))))
         (process
          (make-process
           :name (concat "process-" command)
           :buffer stdout-buffer
           :stderr stderr-process
           :command (cons command args)
           :sentinel wrapped-sentinel)))
    process))

(cl-defun ow-run-command-async-24 (command &rest args &key sentinel &allow-other-keys)
  "Run COMMAND with ARGS asynchronously. SENTINEL runs when processes change status.
Legacy implementation for Emacs < 25. Reminder that kwargs must
come before rest when calling a cl-defun."
  (let* ((args (or (and (memq :sentinel args)
                        (cl-remove-if (lambda (x) (or (not x) (eq x :sentinel)))
                                      (plist-put args :sentinel nil)))
                   args))
         (process (apply #'start-process
                         (format "process-%s" command)
                         (generate-new-buffer
                          (format " process-buffer-%s" command))
                         command
                         args)))
    (when sentinel
      (set-process-sentinel process sentinel))
    process))

(when (< emacs-major-version 25)
  (defalias 'ow-run-command-async #'ow-run-command-async-24))


(defvar securl-default-cypher 'sha256)  ; remember kids, always publish the cypher with the checksum

(defun securl-path-checksum (path &optional cypher)
  "Compute checksum for PATH under CYPHER.
Not as fast as using sha256sum, but requires no dependencies 1.4s vs .25s for ~60mb"
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

(defmacro ow--setq (global &rest body)
  `(if ,global
       (setq ,@body)
     (setq-local ,@body)))

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

(defun ow--results-silent (fun &rest args)
  "Whoever named the original version of this has a strange sense of humor."
  ;; so :results silent, which is what org babel calls between vars
  ;; set automatically is completely broken when one block calls another
  ;; there likely needs to be an internal gensymed value that babel blocks
  ;; can pass to eachother so that a malicious user cannot actually slience
  ;; values, along with an option to still print, but until then we have this
  (let ((result (car args))
        (result-params (cadr args)))
    (if (member "silent" result-params)
        result
      (apply fun args))))

(defun ow-babel-eval (block-name &optional universal-argument)
  "Use to confirm running a chain of dependent blocks starting with BLOCK-NAME.
This retains single confirmation at the entry point for the block."
  ;; TODO consider a header arg for a variant of this in org babel proper
  (interactive "P")
  (let ((org-confirm-babel-evaluate (lambda (_l _b) nil))) ;; FIXME TODO set messages buffer size to nil
    (save-excursion
      (when (org-babel-find-named-block block-name)
        ;; goto won't raise an error which results in the block where
        ;; `ow-confirm-once' is being used being called an infinite
        ;; number of times and blowing the stack
        (org-babel-goto-named-src-block block-name)
        (unwind-protect
            (progn
              ;; FIXME optionally raise errors on failure here !?
              (advice-add #'org-babel-insert-result :around #'ow--results-silent)
              (org-babel-execute-src-block))
          (advice-remove #'org-babel-insert-result #'ow--results-silent))))))

(defvar ow-package-archives '(("gnu" . "https://elpa.gnu.org/packages/") ; < 26 has http
                              ("melpa" . "https://melpa.org/packages/")
                              ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(when (< emacs-major-version 26)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(defun ow-enable-use-package ()
  "Do all the setup needed for `use-package'.
This needs to be called with (eval-when-compile ...) to the top level prior
to any use of `use-package' otherwise it will be missing and fail"
  ;; package-archives is not an argument to this function to ensure that
  ;; there is only one place that needs to be updated if an archive location
  ;; changes, namely this library, updating that is easier to get right using
  ;; the `reval-update' machinery
  (require 'package)
  (when (< emacs-major-version 26)
    (setq package-archives
          (cl-remove-if (lambda (p) (equal p '("gnu" . "http://elpa.gnu.org/packages/")))
                        package-archives))
    (add-to-list 'package-archives (assoc "gnu" ow-package-archives))
    (package-initialize)
    (unless (package-installed-p 'gnu-elpa-keyring-update)
      (let (os package-check-signature)
        (setq package-check-signature nil)
        (package-refresh-contents)
        (package-install 'gnu-elpa-keyring-update)
        (warn "You need to restart Emacs for package keyring changes to take effect.")
        (setq package-check-signature os)))
    (setq package--initialized nil))
  (dolist (pair ow-package-archives)
    (add-to-list 'package-archives pair t))
  (unless package--initialized
    (package-initialize))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

(defmacro ow-use-packages (&rest names)
  "enable multiple calls to `use-package' during bootstrap
additional configuration can be provided by converting the symbol
into a list (name body ...)"
  (cons
   'progn
   (mapcar (lambda (name)
             (cond ((symbolp name) `(use-package ,name))
                   ((listp name)
                    (unless (eq (car name) 'quote)
                      (if (memq (car name) '(if when unless))
                          `(,(car name) ,(cadr name) (use-package ,@(cddr name)))
                        `(use-package ,@name))))
                   ((t (error "unhandled type %s" (type-of name))))))
           names)))

;; ow-cli

(require 'cl-lib)

(defun ow-string-to-number (string &optional base)
  "vanilla `string-to-number' has a degenerate case with \"0\""
  (let ((maybe-zero (string-to-number string base)))
    (if (= maybe-zero 0)
        (if (string= maybe-zero "0")
            0
          (error "%S is not a number!" string))
      maybe-zero)))

(defun ow-keyword-name (keyword)
  "Get the `symbol-name' of KEYWORD without the leading colon."
  (unless (keywordp keyword)
    (error "%s is not a keyword! %s" keyword (type-of keyword)))
  (substring (symbol-name keyword) 1))

(defun ow-cli--norm-arg (arg)
  (let ((int (ignore-errors (ow--string-to-number arg))))
    (if int int arg)))

(defun ow-cli--process-bind-keyword (bind-keyword)
  "Processes BIND-KEYWORD into let-binding elements `cl-case' elements and alist elements.

Bind keyword lists may take the following forms.

(:flag) ; legacy support before we added the internal binding clause
((:flag)) ; same as (:flag)
((:flag) flag-internal)

(:option default)
((:option default))
((:option default) option-internal)"
  (unless (listp bind-keyword)
    (error "%s not a list! %s" bind-keyword (type-of bind-keyword)))
  (let* ((kw-or-element? (car bind-keyword))
         (bind? (if (keywordp kw-or-element?) nil (cdr bind-keyword)))
         (element (if (keywordp kw-or-element?) bind-keyword kw-or-element?))
         (_ (unless (listp element)
              (error "%s not a list! %s" element (type-of element))))
         (kw (car element))
         (sl (ow-keyword-name kw))
         (assign? (cdr element))
         (real-assign (if bind? (car bind?) (intern (ow-keyword-name kw))))
         (default (if assign? (car assign?) assign?)) ; FIXME
         (p (if assign?
                `(progn (setf ,real-assign (ow-cli--norm-arg (cadr args)))
                        ;; equivalent of bash shift shift
                        (setf args (cddr args)))
              `(progn (setf ,real-assign t)
                      ;; equivalent of bash shift
                      (setf args (cdr args))))))
    (list `(,real-assign ,default)  ; default
          `(,(intern (format "--%s" sl)) ,p)  ; case
          `(cons ',real-assign ,real-assign))))

(defmacro ow-cli-parse-args (&rest keywords)
  "(parse-args (:port port) (:pid pid) (:flag))

   XXX This is a legacy function.

   NOTE if the default value if a kwarg is nil rather than
   empty i.e. (:asdf nil) vs (:asdf) the form with nil will
   not fail but will be nil unless some value is provided
   AND it will eat the next kwarg this is probably a misdesign"
  `(ow-cli-gen ,keywords parsed))

(defmacro ow-cli-gen (bind-keywords &rest body) ; (ref:cli-gen)
  "All the machinery needed for simple cli specification.

BIND-KEYWORDS follow a reverse let pattern because if the name to
bind is not specified then it is the `ow-keyword-name' of the keyword
used to specify the command line option.

For example
((:option default)) -> --option value -> (let ((option \"value\")) )
((:option default) option-internal) -> --option value -> (let ((option-internal \"value\")) )"
;; FIXME ambiguity between (:option bind-to-name) and ((:option) bind-to-name)
  (declare (indent 2) (indent 1))
  (cl-destructuring-bind (defaults cases returns)
      (apply #'cl-mapcar #'list ; `cl-mapcar' required for this to work
             (mapcar #'ow-cli--process-bind-keyword bind-keywords))
    `(let ((args (cdr command-line-args-left))
           ,@defaults)
       (cl-do ()
           ((null args) nil)
         (cl-case (intern (car args))
           ,@cases
           (otherwise (progn (message "unhandled: %s" (car args))
                             (setf args (cdr args))))))
       (let (cases returns (parsed (list ,@returns)))
         ,@body))))

;; orthauth-minimal

(defvar oa-secrets nil "path to orthauth secrets.sxpr file")

(defun oa--resolve-path (plist elements)
  "recursively `cl-getf' in order keywords from ELEMENTS in nested plists inside PLIST"
  (if elements
      (oa--resolve-path (cl-getf plist (car elements)) (cdr elements))
    plist))

(defun oa--read (path)
  "read the first sexpression in the file at PATH"
  (with-temp-buffer
    (insert-file-contents path)
    (read (buffer-string))))

(defun oa-path (&rest elements)
  "Retrieve value at nested path defined by keywords provided in ELEMENTS in `oa-secrets'"
  (let ((plist (oa--read oa-secrets)))
    (oa--resolve-path plist elements)))

;; control initial visibility

(defun ow-hide-section-0-blocks ()
  "Hide blocks and dynamic blocks that are used in section 0."
  (let ((dblocks '("metadata" "properties" "prefixes"))
        (blocks '("orgstrap-shebang")))
    ;; dblocks and blocks have separate namespaces
    (save-excursion
      (mapcar (lambda (name) (and (org-find-dblock name) (org-hide-block-toggle 'hide)))
              dblocks)
      ;; FIXME inconsistent behavior between `org-find-dblock' and `org-babel-find-named-block'
      (mapcar (lambda (name)
                (let ((p (org-babel-find-named-block name)))
                  (and p (goto-char p) (org-hide-block-toggle 'hide))))
              blocks))))

;; permanently modify visibility

(defun ow-fold-headline (&optional name)
  "Set visibility property of headline with NAME or previous visible to folded."
  ;; https://orgmode.org/manual/Using-the-Property-API.html
  (save-excursion
    (if name
        (goto-char (org-find-exact-headline-in-buffer name))
      (org-previous-visible-heading 0))
    (org-entry-put nil "visibility" "folded")
    (save-buffer)))

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

(defun ow--set-mouse-cycle ()
  "Hook fun to set mouse-cycle behavior for org buffers."
  (local-unset-key [mouse-1])
  (local-set-key [mouse-1] #'ow--safe-cycle))

(defun ow-enable-mouse-cycle (&optional global)
  "Allow left mouse to cycle org headings.
Set GLOBAL to enable for all org buffers."
  (interactive)
  ;; reset `org-cycle-hook' as a local variable so that
  ;; we can add/remove individual hooks without messing
  ;; with the global behavior which might some day not
  ;; be purely single threaded (heh)
  (setq-local org-cycle-hook org-cycle-hook)
  (ow--set-mouse-cycle)
  (when global
    (add-hook 'org-mode-hook #'ow--set-mouse-cycle)))

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

(defun ow--org-link-set-parameters (type &rest parameters)
  "no-op to prevent error, install a newer version of org or emacs")

(defun ow-make-buttons ()
  "Enable standard buttons." ; needed to avoid autoloading the built-in version of org-mode

  (when (string< "9.3" (org-version))
    ;; before 9.3 the org link functionality was still in org.el
    (require 'ol))

  (when (string< (org-version) "9.0")
    (defalias 'org-link-set-parameters #'ow--org-link-set-parameters))

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
                             (ow-recenter-on-mouse))))

(defun ow--headline-faces ()
  "Set face for all headline levels to be bold and 1.2x as tall."
  (mapcar (lambda (n) (set-face-attribute (intern (format "org-level-%s" n)) nil :bold t :height 1.2))
          (number-sequence 1 8)))

(defun ow--tweak-whiteboard ()
  "Tweak the settings for `whiteboard-theme'."
  (require 'org-faces)
  (set-face-attribute 'shadow nil :foreground "gray35")
  (set-face-attribute 'org-meta-line nil :inherit font-lock-keyword-face)
  (let ((dx (>= emacs-major-version 27)))
    (apply #'set-face-attribute `(org-block-begin-line nil :foreground "black" :background "silver" ,@(when dx '(:extend t))))
    (apply #'set-face-attribute `(org-block-end-line nil :foreground "black" :background "silver" ,@(when dx '(:extend t))))
    (apply #'set-face-attribute `(org-block nil :background "white" ,@(when dx '(:extend t))))))

(defun ow--rainy-day ()
  "Enable `rainbow-deimiters-mode' with tweaks."
  (ow-use-packages (rainbow-delimiters :hook ((prog-mode) . rainbow-delimiters-mode)))
  (set-face-attribute 'rainbow-delimiters-base-face nil :bold t)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :bold t :foreground "white" :background "red")
  (set-face-attribute 'rainbow-delimiters-mismatched-face nil :bold t :foreground "black" :background "yellow"))

(defun ow-enable-config-familiar-1 (&optional global)
  "Minimal config to achieve something more familiar for non-Emacs users.

Uses `cua-mode' with additional tweak for undo bindings.
NOTE: `undo-fu' is required for Emacs < 28."

  ;; Enable familiar copy/paste keybindings
  (cua-mode t)

  ;; additional keybinds
  (let ((set-key (if global #'global-set-key #'local-set-key)))
    ;; Ctrl s for save
    (funcall set-key (kbd "C-s") #'save-buffer)
    ;; Ctrl f for find aka isearch
    (funcall set-key (kbd "C-f") #'isearch-forward)
    ;; enable Cmd Shift Z for apple users Ctrl y for windows
    (when (fboundp #'undo-redo)
      (if (eq system-type 'darwin)
          (funcall set-key (kbd "C-Z") #'undo-redo)
        (funcall set-key (kbd "C-y") #'undo-redo))))

  ;; Move text smoothly when point is at top or bottom of buffer
  (ow--setq global scroll-conservatively 101)
  (ow--setq global scroll-step 1)

  ;; Use left mouse to cycle
  (ow-enable-mouse-cycle)

  ;; Mouse paste at point not cursor
  (setq mouse-yank-at-point t) ; set globally due to minibuffer

  ;; Mouse wheel behavior
  (ow--setq global mouse-wheel-progressive-speed nil)
  (ow--setq global mouse-wheel-scroll-amount '(3 ((shift) . hscroll)))

  ;; Mouse on scroll bar behavior TODO this is not quite right, but I
  ;; have no idea how to get emacs to stop resizing the sliders
  (global-unset-key [vertical-scroll-bar mouse-1])
  (global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)

  ;; default shift functionality is usually not needed in ow files and
  ;; the message when you try to use it can be extremely confusing
  (ow--setq global org-support-shift-select t)

  ;; Enable tab-bar-mode
  (when (>= emacs-major-version 27)
    (tab-bar-mode t))

  ;; Use the whiteboard theme
  (load-theme 'whiteboard)
  (ow--tweak-whiteboard)

  ;; Set headline faces
  (ow--headline-faces))

(defun ow--reval-update ()
  "Get the immutable url for the current remote version of this file."
  (reval-get-imm-github "tgbugs" "orgstrap" "ow.el"))

(provide 'ow)

;;; ow.el ends here
