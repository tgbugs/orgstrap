;;; defl.el --- Buffer local functions. -*- lexical-binding: t -*-

;; Author: Tom Gillespie
;; Homepage: https://github.com/tgbugs/orgstrap
;; Version: 9999
;; Package-Requires: ((emacs "24.4"))
;; Is-Version-Of: https://raw.githubusercontent.com/tgbugs/orgstrap/master/defl.el
;; Reval-Get-Immutable: defl--reval-update

;;;; License and Commentary

;; License:
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This is a horribly hacked implementation of buffer local functions.
;; The primary use case is to make it possible to define file local
;; functions for orgstrap blocks for use in closures in the org file
;; itself. Having buffer local functions in this context vasly simplifies
;; the issue of potential name collisions for functions that have short
;; names but different definitions between different files. If elisp had
;; namespaces this wouldn't be an issue, for the orgstrap use case buffer
;; local is good enough to prevent accidental redefinition.

;; defl.el is compatible with `reval-update'.

;;; Code:

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

(defun defl--reval-update ()
  "Get the immutable url for the current remote version of this file."
  (reval-get-imm-github "tgbugs" "orgstrap" "defl.el"))

(provide 'defl)

;;; defl.el ends here
