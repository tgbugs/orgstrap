;;; ow-min.el --- Minimal utilties for orgstrap blocks. -*- lexical-binding: t -*-

;; Author: Tom Gillespie
;; Homepage: https://github.com/tgbugs/orgstrap
;; Version: 9999
;; Package-Requires: ((emacs "24.4"))
;; Is-Version-Of: https://raw.githubusercontent.com/tgbugs/orgstrap/master/ow-min.el
;; Reval-Get-Immutable: ow-min--reval-update

;;;; License and Commentary

;; License:
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ow-min.el contains functionality needed by orgstrap blocks that are
;; primarily used by developers, such as files that implement a
;; release process where no end user interaction is expected.

;; ow-min.el is compatible with `reval-update'.

;;; Code:

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
          (while (accept-process-output process)) ; don't use mutexes kids
          (let ((ex (process-exit-status process)))
            (if (= 0 ex)
                (with-current-buffer stdout-buffer (buffer-string))
              (error "Command %s failed code: %s stdout: %S stderr: %S"
                     command ex
                     (with-current-buffer stdout-buffer (buffer-string))
                     (with-current-buffer stderr-buffer (buffer-string))))))
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


(defalias 'run-command #'ow-run-command)

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
  (let ((org-confirm-babel-evaluate (lambda (_l _b) nil)))
    (save-excursion
      (when (org-babel-find-named-block block-name)
        ;; goto won't raise an error which results in the block where
        ;; `ow-confirm-once' is being used being called an infinite
        ;; number of times and blowing the stack
        (org-babel-goto-named-src-block block-name)
        (unwind-protect
            (progn
              (advice-add #'org-babel-insert-result :around #'ow--results-silent)
              (org-babel-execute-src-block))
          (advice-remove #'org-babel-insert-result #'ow--results-silent))))))

(defun ow-min--reval-update ()
  "Get the immutable url for the current remote version of this file."
  (reval-get-imm-github "tgbugs" "orgstrap" "ow-min.el"))

(provide 'ow-min)

;;; ow-min.el ends here
