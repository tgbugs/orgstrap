;;; orgstrap.el --- Bootstrap an Org file using file local variables -*- lexical-binding: t -*-

;; Author: Tom Gillespie
;; URL: https://github.com/tgbugs/orgstrap
;; Keywords: lisp org org-mode bootstrap
;; Version: 1.0
;; Package-Requires: ((emacs "24.4"))

;;;; License and Commentary

;; License:
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; orgstrap is a specification and tooling for bootstrapping Org files.

;; It allows Org files to describe their own requirements, and
;; define their own functionality, making them self-contained,
;; standalone computational artifacts, dependent only on Emacs,
;; or other implementations of the Org-babel protocol in the future.

;; orgstrap.el is an elisp implementation of the orgstrap conventions.
;; It defines a regional minor mode for `org-mode' that runs orgstrap
;; blocks.  It also provides `orgstrap-init' and `orgstrap-edit-mode'
;; to simplify authoring of orgstrapped files.  For more details see
;; README.org which is also the literate source for this orgstrap.el
;; file in the git repo at
;; https://github.com/tgbugs/orgstrap/blob/master/README.org
;; or whever you can find git:c1b28526ef9931654b72dff559da2205feb87f75

;; Code in an orgstrap block is usually meant to be executed directly by its
;; containing Org file.  However, if the code is something that will be reused
;; over time outside the defining Org file, then it may be better to tangle and
;; load the file so that it is easier to debug/xref functions.  The code in
;; this orgstrap.el file in particular is tangled for inclusion in one of the
;; *elpas so as to protect the orgstrap namespace and to make it eaiser to
;; use orgstrap in Emacs.

;; The license for the orgstrap.el code reflects the fact that the
;; code for expanding and hashing blocks reuses code from ob-core.el,
;; which at the time of writing is licensed as part of Emacs.

;;; Code:

(require 'org)

(require 'cl-lib)

(defvar orgstrap-mode nil
  "Variable to track whether `orgstrap-mode' is enabled.")

(defvar orgstrap-orgstrap-block-name "orgstrap"
  "Set the default blockname to orgstrap by convention.
This makes it easier to search for orgstrap if someone encounters
an orgstrapped file and wants to know what is going on.")

(defvar orgstrap-default-cypher 'sha256
  "The default cypher passed to `secure-hash' when hashing blocks.")

(defvar-local orgstrap-cypher orgstrap-default-cypher
  "Local variable for the cypher for the current buffer.
If you change `orgstrap-default-cypher' you should update this as well
using `setq-default' since it will not change automatically.")
(put 'orgstrap-cypher 'safe-local-variable (lambda (v) (ignore v) t))

(defvar-local orgstrap-block-checksum nil
  "Local variable for the expected checksum for the current orgstrap block.")
(put 'orgstrap-cypher 'safe-local-variable (lambda (v) (ignore v) t))

(defconst orgstrap--internal-norm-funcs
  '(orgstrap-norm-func--prp-1.0)
  "List internally implemented normalization functions.
Used to determine which norm func names are safe local variables.")

(defvar-local orgstrap-norm-func-name nil
  "Local variable for the name of the current orgstrap-norm-func.")
(put 'orgstrap-norm-func-name 'safe-local-variable
     (lambda (value) (and orgstrap-mode (memq value orgstrap--internal-norm-funcs))))
;; Unless orgstrap-mode is enabled and the name is in the list of
;; functions that are implemented internally this is not safe

(defvar orgstrap-norm-func #'orgstrap-norm-func--prp-1.0
  "Dynamic variable to simplify calling normalizaiton functions.
Defaults to `orgstrap-norm-func--prp-1.0'.")

(defvar orgstrap--debug nil
  "If non-nil run `orgstrap-norm' in debug mode.")

(defcustom orgstrap-always-edit nil
  "If non-nil then command `orgstrap-mode' will activate command `orgstrap-edit-mode'."
  :type 'boolean
  :group 'orgstrap)

;; orgstrap run helpers

;;;###autoload
(defun orgstrap--confirm-eval-portable (lang _body)
  "A backwards compatible, portable implementation for confirm-eval.
This should be called by `org-confirm-babel-evaluate'.  As implemented
the only LANG that is supported is emacs-lisp or elisp.  The argument
_BODY is rederived for portability and thus not used."
  ;; `org-confirm-babel-evaluate' will prompt the user when the value
  ;; that is returned is non-nil, therefore we negate positive matchs
  (not (and (member lang '("elisp" "emacs-lisp"))
            (let* ((body (orgstrap--expand-body (org-babel-get-src-block-info)))
                   (body-normalized (orgstrap-norm body))
                   (content-checksum
                    (intern
                     (secure-hash
                      orgstrap-cypher
                      body-normalized))))
              ;;(message "%s %s" orgstrap-block-checksum content-checksum)
              ;;(message "%s" body-normalized)
              (eq orgstrap-block-checksum content-checksum)))))
;; portable eval is used as the default implementation in orgstrap.el
;;;###autoload
(defalias 'orgstrap--confirm-eval #'orgstrap--confirm-eval-portable)

;; orgstrap-mode implementation

(defun orgstrap--org-buffer ()
  "Only run when in `org-mode' and command `orgstrap-mode' is enabled.
Sets further hooks."
  (advice-add #'hack-local-variables-confirm :around #'orgstrap--hack-lv-confirm)
  (add-hook 'before-hack-local-variables-hook #'orgstrap--before-hack-lv nil t))

(defun orgstrap--hack-lv-confirm (command &rest args)
  "Advise `hack-local-variables-confirm' to remove orgstrap eval variables.
COMMAND should be `hack-local-variables-confirm' with ARGS (all-vars
unsafe-vars risky-vars dir-name)."
  (advice-remove #'hack-local-variables-confirm #'orgstrap--hack-lv-confirm)
  (cl-destructuring-bind (all-vars unsafe-vars risky-vars dir-name)
      ;; emacs 28 doesn't alias the non cl- prefixed form so use unaliased?
      (mapcar (lambda (arg)
                (if (listp arg)
                    ;; We must use `cl-delete-if-not' on all-vars,
                    ;; otherwise the list pointed to by all-vars in
                    ;; the calling scope will remain unmodified and
                    ;; the eval variable will be run without being
                    ;; checked or confirmed. This also spills over to
                    ;; the other -vars which is extra insurance
                    ;; against any future changes to the
                    ;; implementation in the calling scope.
                    (cl-delete-if-not #'orgstrap--match-eval-local-variables arg)
                  arg))
              args)
    ;; After removal we have to recheck to see if unsafe-vars and
    ;; risky-vars are empty so we can skip the confirm dialogue. If we
    ;; do not, then the dialogue breaks the flow.
    (or (and (null unsafe-vars)
             (null risky-vars))
        (funcall command all-vars unsafe-vars risky-vars dir-name))))

(defun orgstrap--before-hack-lv ()
  "If `orgstrap' is in the current buffer, add hook to run the orgstrap block."
  ;; This approach is safer than trying to introspect some of the implementation
  ;; internals. This hook will only run if there are actually local variables to
  ;; hack, so there is little to no chance of lingering hooks if an error occures
  (remove-hook 'before-hack-local-variables-hook #'orgstrap--before-hack-lv t)
  (add-hook 'hack-local-variables-hook #'orgstrap--hack-lv nil t))

(defun orgstrap--used-in-current-buffer-p ()
  "Return t if all the required orgstrap prop line local variables are present."
  (and (boundp 'orgstrap-cypher) orgstrap-cypher
       (boundp 'orgstrap-block-checksum) orgstrap-block-checksum
       (boundp 'orgstrap-norm-func-name) orgstrap-norm-func-name))

(defmacro orgstrap--lv-common-with-block-name ()
  "Helper macro to allow use of same code between core and lv impls."
  ` ; separate line to avoid the issue with noweb and prefixes
  (let ((ocbe org-confirm-babel-evaluate))
    (setq-local orgstrap-norm-func orgstrap-norm-func-name)
    (setq-local org-confirm-babel-evaluate #'orgstrap--confirm-eval)
    (unwind-protect
        (save-excursion
          (org-babel-goto-named-src-block ,orgstrap-orgstrap-block-name) ; quasiquoted when nowebbed
          (org-babel-execute-src-block))
      (setq-local org-confirm-babel-evaluate ocbe))))

(defun orgstrap--hack-lv ()
  "If orgstrap is present, run the orgstrap block for the current buffer."
  (remove-hook 'hack-local-variables-hook #'orgstrap--hack-lv)
  (when (orgstrap--used-in-current-buffer-p)
    (orgstrap--lv-common-with-block-name)
    (when orgstrap-always-edit
      (orgstrap-edit-mode))))

(defun orgstrap--match-eval-local-variables (pair)
  "Return nil if PAIR matchs any eval local variable used by orgstrap.
Avoid false positives if possible if at all possible."
  (not (and (eq (car pair) 'eval)
            ;;(message "%s" (cdr pair))
            ;; keep the detection simple for now, any eval lv that
            ;; so much as mentions orgstrap is nuked, and in the future
            ;; if orgstrap-nb is used we may need to nuke that too
            (string-match "orgstrap" (prin1-to-string (cdr pair))))))

;;;###autoload
(defun orgstrap-mode (&optional arg)
  "A regional minor mode for `org-mode' that automatically runs orgstrap blocks.
When visiting an Org file or activating `org-mode', if orgstrap prop line local
variables are detect then use the installed orgstrap implementation to run the
orgstrap block.  If orgstrap embedded local variables are present, they will not
be executed.  `orgstrap-mode' is not a normal minor mode since it does not run
any hooks and when enabled only adds a function to `org-mode-hook'.  ARG is the
universal prefix argument."
  (interactive "P")
  (ignore arg)
  (let ((turn-on (not orgstrap-mode)))
    (cond (turn-on
           ;;(unless (boundp 'orgstrap-orgstrap-block-name)
           ;;  (require 'orgstrap))
           (add-hook 'org-mode-hook #'orgstrap--org-buffer)
           (setq orgstrap-mode t)
           (message "orgstrap-mode enabled"))
          (t
           (remove-hook 'org-mode-hook #'orgstrap--org-buffer)
           (setq orgstrap-mode nil)
           (message "orgstrap-mode disabled")))))

;;; edit helpers
(defcustom orgstrap-on-change-hook nil
  "Hook run via `before-save-hook' when command `orgstrap-edit-mode' is enabled.
Only runs when the contents of the orgstrap block have changed."
  :type 'hook
  :group 'orgstrap)

(defcustom orgstrap-use-minimal-local-variables nil
  "Set whether minimal, smaller but less portable variables are used.
If nil then backward compatible local variables are used instead.
If the value is customized to be non-nil then compact local variables
are used and `orgstrap-min-org-version' is set accordingly.  If the
current version of org mode does not support the features required to
use the minimal variables then the portable variables are used instead."
  :type 'boolean
  :group 'orgstrap)

;; edit utility functions
(defun orgstrap--current-buffer-cypher ()
  "Return the cypher used for the current buffer.
The value is `orgstrap-cypher' if it is bound otherwise
`orgstrap-default-cypher' is returned."
  (if (boundp 'orgstrap-cypher) orgstrap-cypher orgstrap-default-cypher))

(defun orgstrap-org-src-coderef-regexp (_fmt &optional label)
  "Backport `org-src-coderef-regexp' for 24 and 25.
See the upstream docstring for info on LABEL.
_FMT has the wrong meaning in 24 and 25."
  (let ((fmt org-coderef-label-format))
    (format "\\([:blank:]*\\(%s\\)[:blank:]*\\)$"
            (replace-regexp-in-string
             "%s"
             (if label
                 (regexp-quote label)
               "\\([-a-zA-Z0-9_][-a-zA-Z0-9_ ]*\\)")
             (regexp-quote fmt)
             nil t))))
(unless (fboundp #'org-src-coderef-regexp)
  (defalias 'org-src-coderef-regexp #'orgstrap-org-src-coderef-regexp))
(defun orgstrap--expand-body (info)
  "Expand noweb references in INFO body and remove any coderefs."
  ;; this is a backport of `org-babel--expand-body'
  (let ((coderef (nth 6 info))
        (expand
         (if (org-babel-noweb-p (nth 2 info) :eval)
             (org-babel-expand-noweb-references info)
           (nth 1 info))))
    (if (not coderef)
        expand
      (replace-regexp-in-string
       (org-src-coderef-regexp coderef) "" expand nil nil 1))))

(defun orgstrap-norm (body)
  "Normalize BODY."
  (if orgstrap--debug
      (orgstrap-norm-debug body)
    (funcall orgstrap-norm-func body)))

(defun orgstrap-norm-debug (body)
  "Insert BODY normalized with NORM-FUNC into a buffer for easier debug."
  (let* ((print-quoted nil)
         (bname (format "body-norm-%s" emacs-major-version))
         (buffer (let ((existing (get-buffer bname)))
                   (if existing existing
                     (create-file-buffer bname))))
         (body-normalized (funcall orgstrap-norm-func body)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert body-normalized))
    body-normalized))

;; orgstrap normalization functions

(defun orgstrap-norm-func--prp-1.0 (body)
  "Normalize BODY using prp-1.0."
  (let ((print-quoted nil))
    (prin1-to-string (read (concat "(progn\n" body "\n)")))))

(defmacro orgstrap--with-block (blockname &rest macro-body)
  "Go to the source block named BLOCKNAME and execute MACRO-BODY.
The macro provides local bindings for four names:
`info', `params', `body-unexpanded', and `body'."
  (declare (indent defun))
  ;; consider accepting :lite or a keyword or something to pass
  ;; lite as an optional argument to `org-babel-get-src-block-info'
  ;; e.g. via (lite (equal (car macro-body) :lite)), given the
  ;; behavior when lite is not nil and the expected useage of this
  ;; macro I don't think we would ever want to pass a non nil lite
  `(save-excursion
     (let ((inhibit-message t)) ; inhibit-message only blocks from the message area not the log
       (org-babel-goto-named-src-block ,blockname))
     (unwind-protect
         (let* ((info (org-babel-get-src-block-info))
                (params (nth 2 info))
                (body-unexpanded (nth 1 info))
                ;; from `org-babel-check-confirm-evaluate'
                ;; and `org-babel-execute-src-block'
                (body (orgstrap--expand-body info)))
           ,@macro-body)
       ;; `ignore-errors' is needed for cases where this macro
       ;; is used before the buffer is fully set up
       (ignore-errors (org-mark-ring-goto)))))

(defun orgstrap--update-on-change ()
  "Run via the `before-save-hook' local variable.
Test if the checksum of the orgstrap block has changed,
if so update the `orgstrap-block-checksum' local variable
and then run `orgstrap-on-change-hook'."
  (let* ((elv (orgstrap--read-current-local-variables))
         (cpair (assoc 'orgstrap-block-checksum elv))
         (checksum-existing (if cpair (cdr cpair) nil))
         (checksum (orgstrap-get-block-checksum)))
    (unless (eq checksum-existing (intern checksum))
      (remove-hook 'before-save-hook #'orgstrap--update-on-change t)
      ;; for some reason tangling from a buffer counts as saving from that buffer
      ;; so have to remove the hook to avoid infinite loop
      (unwind-protect
          (save-excursion
            ;; using save-excusion here is a good for insurance against wierd hook issues
            ;; however it does not deal with the fact that updating `orgstrap-add-block-checksum'
            ;; adds an entry to the undo ring, which is bad
            ;;(undo-boundary)  ; undo-boundary doesn't quite work the way we want
            ;; related https://emacs.stackexchange.com/q/7558
            (orgstrap-add-block-checksum nil checksum)
            (run-hooks 'orgstrap-on-change-hook))
        (add-hook 'before-save-hook #'orgstrap--update-on-change nil t)))))

;; edit user facing functions
(defun orgstrap-get-block-checksum (&optional cypher)
  "Calculate the `orgstrap-block-checksum' for the current buffer using CYPHER."
  (interactive)
  (orgstrap--with-block orgstrap-orgstrap-block-name
    (ignore params body-unexpanded)
    (let ((cypher (or cypher (orgstrap--current-buffer-cypher)))
          (body-normalized (orgstrap-norm body)))
      (secure-hash cypher body-normalized))))

(defun orgstrap-add-block-checksum (&optional cypher checksum)
  "Add `orgstrap-block-checksum' to file local variables of `current-buffer'.
The optional CYPHER argument should almost never be used,
instead change the value of `orgstrap-default-cypher' or manually
change the file property line variable.  CHECKSUM can be passed
directly if it has been calculated before and only needs to be set."
  (interactive)
  (let* ((cypher (or cypher (orgstrap--current-buffer-cypher)))
         (orgstrap-block-checksum (or checksum (orgstrap-get-block-checksum cypher))))
    (when orgstrap-block-checksum
      (save-excursion
        (add-file-local-variable-prop-line 'orgstrap-cypher         cypher)
        (add-file-local-variable-prop-line 'orgstrap-norm-func-name orgstrap-norm-func)
        (add-file-local-variable-prop-line 'orgstrap-block-checksum (intern orgstrap-block-checksum))))
    orgstrap-block-checksum))

(defun orgstrap-run-block ()
  "Evaluate the orgstrap block for the current buffer."
  ;; bind to :orb or something like that
  (interactive)
  (save-excursion
    (org-babel-goto-named-src-block orgstrap-orgstrap-block-name)
    (org-babel-execute-src-block)))

;;;###autoload
(define-minor-mode orgstrap-edit-mode
  "Minor mode for editing with orgstrapped files."
  nil "" nil

  (unless (eq major-mode 'org-mode)
    (setq orgstrap-edit-mode nil)
    (user-error "`orgstrap-edit-mode' only works with org-mode buffers"))

  (cond (orgstrap-edit-mode
         (add-hook 'before-save-hook #'orgstrap--update-on-change nil t))
        (t
         (remove-hook 'before-save-hook #'orgstrap--update-on-change t))))

;;; init helpers
(defvar orgstrap-link-message "jump to the orgstrap block for this file"
  "Default message for file internal links.")

(defvar-local orgstrap--local-variables nil
  "Variable to capture local variables from `hack-local-variables'.")

;; local variable generation functions

(defun orgstrap--get-min-org-version (info minimal)
  "Get minimum org mode version needed by the orgstrap block for this file.
INFO is the source block info.  MINIMAL sets whether to use minimal local vars."
  (if minimal
      (let ((coderef (or (nth 6 info) org-coderef-label-format))
            (noweb (org-babel-noweb-p (nth 2 info) :eval)))
        (if noweb
            "9.3.8"
          (let* ((body (or (nth 1 info) ""))
                 (crrx (org-src-coderef-regexp coderef))
                 (pos (string-match crrx body))
                 (commented
                  (and pos (string-match
                            (concat (rx ";" (zero-or-more whitespace)) crrx) body))))
            ;; FIXME the right way to do this is similar to what is done in
            ;; `org-export-resolve-coderef' but for now we know we are in elisp
            (if (or (not pos) commented)
                "8.2.10"
              "9.3.8"))))
    "8.2.10"))

(defun orgstrap--have-min-org-version (info minimal)
  "See if current version of org meets minimum requirements for orgstrap block.
INFO is the source block info.
MINIMAL is passed to `orgstrap--get-min-org-version'."
  (let ((actual (org-version))
        (need (orgstrap--get-min-org-version info minimal)))
    (or (not need)
        (string< need actual)
        (string= need actual))))

(defun orgstrap--dedoc (sexp)
  "Remove docstrings from SEXP."
  ;; defun 3 defmacro 3 defvar 3
  (if (symbolp (elt sexp 0))
      (if (and (memq (elt sexp 0) '(defun defmacro defvar))
               (stringp (elt sexp 3))
               (or (eq (elt sexp 0) 'defvar)
                   (elt sexp 4)))
          (append (cl-subseq sexp 0 3) (cl-subseq sexp 4))
        sexp)
    (mapcar #'orgstrap--dedoc sexp)))

(defun orgstrap--local-variables--check-version (info &optional minimal)
  "Return the version check local variables given INFO and MINIMAL."
  `(
    (setq-local orgstrap-min-org-version ,(orgstrap--get-min-org-version info minimal))
    (let ((actual (org-version))
          (need orgstrap-min-org-version))
      (or (fboundp #'orgstrap--confirm-eval) ; orgstrap with portable is already present on the system
          (not need)
          (string< need actual)
          (string= need actual)
          (error "Your Org is too old! %s < %s" actual need)))))

(defun orgstrap--local-variables--norm (&optional norm-func-name)
  "Return the normalization function for local variables given NORM-FUNC-NAME."
  (let ((norm-func-name (or norm-func-name orgstrap-norm-func)))
    (cl-case norm-func-name
      (orgstrap-norm-func--prp-1.0
       '(
         (defun orgstrap-norm-func--prp-1.0 (body)
           "Normalize BODY using prp-1.0."
           (let ((print-quoted nil))
             (prin1-to-string (read (concat "(progn\n" body "\n)")))))))
      (otherwise (error "Don't know that normalization function %s" norm-func-name)))))

(defun orgstrap--local-variables--norm-common ()
  "Return the common normalization functions for local variables."
  '(
    (unless (boundp 'orgstrap-norm-func)
      (defvar orgstrap-norm-func orgstrap-norm-func-name))
    
    (defun orgstrap-norm-embd (body)
      "Normalize BODY."
      (funcall orgstrap-norm-func body))
    
    (unless (fboundp #'orgstrap-norm)
      (defalias 'orgstrap-norm #'orgstrap-norm-embd))))

(defun orgstrap--local-variables--eval (info &optional minimal)
  "Return the portable or MINIMAL eval local variables given INFO."
  (let* ((minimal (or minimal orgstrap-use-minimal-local-variables))
         (minimal (and minimal (orgstrap--have-min-org-version info minimal))))
    (if minimal
        '(
          (defun orgstrap--confirm-eval-minimal (lang body)
            (not (and (member lang '("elisp" "emacs-lisp"))
                      (eq orgstrap-block-checksum
                          (intern
                           (secure-hash
                            orgstrap-cypher
                            (orgstrap-norm body)))))))
          (unless (fboundp 'orgstrap--confirm-eval)
            ;; if `orgstrap--confirm-eval' is bound use it since it is
            ;; is the portable version XXX NOTE the minimal version will
            ;; not be installed as local variables if it detects that there
            ;; are unescaped coderefs since those will cause portable and minimal
            ;; to produce different hashes
            (defalias 'orgstrap--confirm-eval #'orgstrap--confirm-eval-minimal)))
      '(
        (defun orgstrap-org-src-coderef-regexp (_fmt &optional label)
          "Backport `org-src-coderef-regexp' for 24 and 25.
        See the upstream docstring for info on LABEL.
        _FMT has the wrong meaning in 24 and 25."
          (let ((fmt org-coderef-label-format))
            (format "\\([:blank:]*\\(%s\\)[:blank:]*\\)$"
                    (replace-regexp-in-string
                     "%s"
                     (if label
                         (regexp-quote label)
                       "\\([-a-zA-Z0-9_][-a-zA-Z0-9_ ]*\\)")
                     (regexp-quote fmt)
                     nil t))))
        (unless (fboundp #'org-src-coderef-regexp)
          (defalias 'org-src-coderef-regexp #'orgstrap-org-src-coderef-regexp))
        (defun orgstrap--expand-body (info)
          "Expand noweb references in INFO body and remove any coderefs."
          ;; this is a backport of `org-babel--expand-body'
          (let ((coderef (nth 6 info))
                (expand
                 (if (org-babel-noweb-p (nth 2 info) :eval)
                     (org-babel-expand-noweb-references info)
                   (nth 1 info))))
            (if (not coderef)
                expand
              (replace-regexp-in-string
               (org-src-coderef-regexp coderef) "" expand nil nil 1))))

        ;;;###autoload
        (defun orgstrap--confirm-eval-portable (lang _body)
          "A backwards compatible, portable implementation for confirm-eval.
        This should be called by `org-confirm-babel-evaluate'.  As implemented
        the only LANG that is supported is emacs-lisp or elisp.  The argument
        _BODY is rederived for portability and thus not used."
          ;; `org-confirm-babel-evaluate' will prompt the user when the value
          ;; that is returned is non-nil, therefore we negate positive matchs
          (not (and (member lang '("elisp" "emacs-lisp"))
                    (let* ((body (orgstrap--expand-body (org-babel-get-src-block-info)))
                           (body-normalized (orgstrap-norm body))
                           (content-checksum
                            (intern
                             (secure-hash
                              orgstrap-cypher
                              body-normalized))))
                      ;;(message "%s %s" orgstrap-block-checksum content-checksum)
                      ;;(message "%s" body-normalized)
                      (eq orgstrap-block-checksum content-checksum)))))
        ;; portable eval is used as the default implementation in orgstrap.el
        ;;;###autoload
        (defalias 'orgstrap--confirm-eval #'orgstrap--confirm-eval-portable)))))

(defun orgstrap--local-variables--eval-common ()
  "Return the common eval check functions for local variables."
  `( ; quasiquote to fill in `orgstrap-orgstrap-block-name'
    (let ((ocbe org-confirm-babel-evaluate))
      (setq-local orgstrap-norm-func orgstrap-norm-func-name)
      (setq-local org-confirm-babel-evaluate #'orgstrap--confirm-eval)
      (unwind-protect
          (save-excursion
            (org-babel-goto-named-src-block ,orgstrap-orgstrap-block-name) ; quasiquoted when nowebbed
            (org-babel-execute-src-block))
        (setq-local org-confirm-babel-evaluate ocbe)))))

;; init utility functions

(defun orgstrap--new-heading-elisp-block (heading block-name &optional header-args noexport)
  "Create a new elisp block named BLOCK-NAME in a new heading titled HEADING.
The heading is inserted at the top of the current file.
HEADER-ARGS is an alist of symbols that are converted to strings.
If NOEXPORT is non-nil then the :noexport: tag is added to the heading."
  (save-excursion
    (goto-char (point-min))
    (outline-next-heading)  ;; alternately outline-next-heading
    (org-meta-return)
    (insert (format "%s%s\n" heading (if noexport " :noexport:" "")))
    ;;(org-edit-headline heading)
    ;;(when noexport (org-set-tags "noexport"))
    (move-end-of-line 1)
    (insert "\n#+name: " block-name "\n")
    (insert "#+begin_src elisp")
    (mapc (lambda (header-arg-value)
            (insert " :" (symbol-name (car header-arg-value))
                    " " (symbol-name (cdr header-arg-value))))
          header-args)
    (insert "\n#+end_src\n")))

(defun orgstrap--trap-hack-locals (command &rest args)
  "Advice for `hack-local-variables-filter' to do nothing except the following.
Set `orgstrap--local-variables' to the reversed list of read variables which
are the first argument in the lambda list ARGS.
COMMAND is unused since we don't actually want to hack the local variables,
just get their current values."
  (ignore command)
  (setq-local orgstrap--local-variables (reverse (car args)))
  nil)

(defun orgstrap--read-current-local-variables ()
  "Return the local variables for the current file without applying them."
  (interactive)
  ;; orgstrap--local-variables is a temporary local variable that is used to
  ;; capture the input to `hack-local-variables-filter' it is unset at the end
  ;; of this function so that it cannot accidentally be used when it might be stale
  (setq-local orgstrap--local-variables nil)
  (let ((enable-local-variables t))
    (advice-add #'hack-local-variables-filter :around #'orgstrap--trap-hack-locals)
    (unwind-protect
        (hack-local-variables nil)
      (advice-remove #'hack-local-variables-filter #'orgstrap--trap-hack-locals))
    (let ((local-variables orgstrap--local-variables))
      (makunbound 'orgstrap--local-variables)
      local-variables)))

(defun orgstrap--add-link-to-orgstrap-block (&optional link-message)
  "Add an `org-mode' link pointing to the orgstrap block for the current file.
The link is placed in comment on the second line of the file.  LINK-MESSAGE
can be used to override the default value set via `orgstrap-link-message'"
  (interactive)  ; TODO prompt for message with C-u ?
  (goto-char (point-min))
  (next-logical-line)  ; use logical-line to avoid issues with visual line mode
  (let ((link-message (or link-message orgstrap-link-message)))
    (unless (save-excursion (re-search-forward
                             (format "^# \\[\\[%s\\]\\[.+\\]\\]$"
                                     orgstrap-orgstrap-block-name)
                             nil t))
      (insert (format "# [[%s][%s]]\n"
                      orgstrap-orgstrap-block-name
                      (or link-message orgstrap-link-message))))))

(defun orgstrap--add-orgstrap-block ()
  "Add a new elisp source block with #+name: orgstrap to the current buffer.
If a block with that name already exists raise an error."
  (interactive)
  (let ((all-block-names (org-babel-src-block-names)))
    (if (member orgstrap-orgstrap-block-name all-block-names)
        (message "orgstrap block already exists not adding!")
      (orgstrap--new-heading-elisp-block "Bootstrap"
                                         orgstrap-orgstrap-block-name
                                         '((results . none)
                                           (lexical . yes))
                                         t)
      (orgstrap--with-block orgstrap-orgstrap-block-name
        (ignore params body-unexpanded body)
        ;;(error "TODO insert some minimal message or something")
        nil))))

(defun orgstrap--add-file-local-variables (&optional minimal norm-func-name)
  "Add the file local variables needed to make orgstrap work.
MINIMAL is used to control whether the portable or minimal block is used.
If MINIMAL is set but the orgstrap block uses features like noweb and
uncommented coderefs and function `org-version' is too old, then the portable
block will be used.  NORM-FUNC-NAME is an optional argument that can be provided
to determine which normalization function is used independent of the current
buffer or global setting for `orgstrap-norm-func'."
  ;; switching comments probably wont work ? we can try
  ;; Use a prefix argument (i.e. C-u) to add file local variables comments instead of in a :noexport:
  (interactive)
  (let ((info (save-excursion
                (org-babel-goto-named-src-block orgstrap-orgstrap-block-name)
                (org-babel-get-src-block-info)))
        (elv (orgstrap--read-current-local-variables)))
    (let ((lv-cver (orgstrap--local-variables--check-version
                    info
                    minimal))
          (lv-norm (orgstrap--local-variables--norm
                    norm-func-name))
          (lv-ncom (orgstrap--local-variables--norm-common))
          (lv-eval (orgstrap--local-variables--eval
                    info
                    minimal))
          (lv-ecom (orgstrap--local-variables--eval-common)))
      (let ((lv-commands (orgstrap--dedoc (append lv-cver lv-norm lv-ncom lv-eval lv-ecom)))
            (commands-existing (mapcar #'cdr (cl-remove-if-not (lambda (l) (eq (car l) 'eval)) elv)))) ;(ref:clrin)
        (cond ((equal commands-existing lv-commands) nil)
              ((not commands-existing)
               (let ((print-escape-newlines t))  ; needed to preserve the escaped newlines
                 ' ; one sexp per eval line, too hard to manage switching it out
                 (mapcar (lambda (sexp) (add-file-local-variable 'eval sexp)) lv-commands)
                 ;; easier to put it all in a single progn that we can id and swap
                 ;; yes it is harder to read, but that is why we have all the docs
                 ;; FIXME detect if orgstrap-global-mode is enabled and do nothing
                 (add-file-local-variable 'eval (cons 'progn lv-commands))))
              ;; we could try to do something fancy here, but it is much simpler
              ;; to just alert the user and have them fix it than trying to guess
              (t (error "Existing eval commands that do not match the commands to be installed have been detected.  Please remove those commands and run `orgsrap-add-file-local-variables' again or manually add the orgstrap file local variables.  The existing commands are as follows.\n%s" commands-existing)))))))

;; init user facing functions
;;;###autoload
(defun orgstrap-init (&optional prefix-argument)
  "Initialize orgstrap in a buffer and enable command `orgstrap-edit-mode'.
PREFIX-ARGUMENT is essentially minimal from other functions, when non-nil
the minimal local variables will be used if possible."
  (interactive "P")
  (unless (eq major-mode 'org-mode)
    (error "Cannot orgstrap, buffer not in `org-mode' it is in %s!" major-mode))
  ;; TODO option for no link?
  ;; TODO option for local variables in comments vs noexport
  (save-excursion
    (orgstrap--add-orgstrap-block)
    (orgstrap-add-block-checksum)
    (orgstrap--add-link-to-orgstrap-block)
    ;; FIXME sometimes local variables don't populate due to an out of range error
    (orgstrap--add-file-local-variables (or prefix-argument orgstrap-use-minimal-local-variables))
    (orgstrap-edit-mode)))

;;; extra helpers

;; leaving out the -on-open from these vars to reduce typing
;; since these will be used repeatedly
(defvar orgstrap-tangle nil
  "Dynamic variable that by convention can be used inside orgstrap blocks.
It makes it possible to run `org-babel-tangle' only when it is non-nil when set
on the command line when launching Emacs with --batch.  Individual orgstrap
blocks should also define (defvar orgstrap-on-tangle-open nil) if they want
this functionality.")

(defvar orgstrap-test nil
  "Variable to control whether to run tests embedded in an orgstrap file.
If non-nil then load the orgstrap block and run tests.")
;; Running tests via a batch process can be a bit tricky if the test code is also part of the orgstrap block.

(defun orgstrap-update-src-block (name content)
  "Set the content of source block named NAME to string CONTENT.
XXX NOTE THAT THIS CANNOT BE USED WITH #+BEGIN_EXAMPLE BLOCKS."
  ;; FIXME this seems to fail if the existing block is empty?
  ;; or at least adding file local variables fails?
  (let ((block (org-babel-find-named-block name)))
    (if block
        (save-excursion
          (org-babel-goto-named-src-block name)
          (org-babel-update-block-body content))
      (error "No block with name %s" name))))

(defun orgstrap-get-src-block-checksum (&optional cypher)
  "Calculate of the checksum of the current source block using CYPHER."
  (interactive)
  (let* ((info (org-babel-get-src-block-info))
         (params (nth 2 info))
         (body-unexpanded (nth 1 info))
         (body (orgstrap--expand-body info))
         (body-normalized
          (orgstrap-norm body))
         (cypher (or cypher (orgstrap--current-buffer-cypher))))
    (ignore params body-unexpanded)
    (secure-hash cypher body-normalized)))

(defun orgstrap-get-named-src-block-checksum (name &optional cypher)
  "Calculate the checksum of the first sourc block named NAME using CYPHER."
  (interactive)
  (orgstrap--with-block name
    (ignore params body-unexpanded)
    (let ((cypher (or cypher (orgstrap--current-buffer-cypher)))
          (body-normalized
           (orgstrap-norm body)))
      (secure-hash cypher body-normalized))))

(defun orgstrap-run-additional-blocks (&rest name-checksum) ;(ref:oab)
  "Securely run additional blocks in languages other than elisp.
Do this by providing the name of the block and the checksum to be embedded
in the orgstrap block as NAME-CHECKSUM pairs."
  (ignore name-checksum)
  (error "TODO"))

(provide 'orgstrap)

;;; orgstrap.el ends here
