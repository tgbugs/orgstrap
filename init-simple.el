;;;; init-simple.el --- entry point -*- lexical-binding: t -*-

;; Author: Tom Gillespie
;; Homepage: https://github.com/tgbugs/orgstrap
;; Version: 9999
;; Package-Requires: ((emacs "24.4"))
;; Is-Version-Of: https://raw.githubusercontent.com/tgbugs/orgstrap/master/init-simple.el

;;;; License and Commentary

;; License:
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; init-simple.el should be copied into to ~/.emacs.d/init.el on
;; systems that need access to a minimally configured emacs that has
;; the key packages loaded that we need for productively working on
;; random systems. init-simple.el acts as a stub that points to a
;; remote init-content.el file which is what is kept up to date and
;; can be synchronized across multiple deployments. Since we use reval
;; to do this, init-simple.el will only update if `reval-update' is
;; invoked explicitly. For this reason we do not include the machinery
;; for updating init-simple.el itself using reval.

;;; Code:

;; user config dir when testing

(defvar init-simple-testing nil)

(when init-simple-testing
  (defconst working-dir
    (concat (file-name-directory (or load-file-name (buffer-file-name)))
            "emacs-d-testing/" (number-to-string emacs-major-version) "/")
    "Directory where this file is located")

  (defvar init-simple-user-dir (expand-file-name "init-simple-user" working-dir))
  (setq user-emacs-directory init-simple-user-dir)

  (defcustom init-simple-use-default-package-dir nil
    "By default do not use default package dir.")
  (unless init-simple-use-default-package-dir
    (setq package-user-dir (expand-file-name
                                        ; emacs 28 introduces a bunch if incompatible changes
                            (concat "init-simple-elpa-" emacs-version)
                            working-dir))))

;;; load remote code

;; emacs tries to be smart about coding systems for things loaded
;; via url handler mode, and/or github decided to try to be smart
;; and modify the contents of the blob they send based on which os
;; they detect, either way, this breaks reval completely, so we set
;; `prefer-coding-system' to avoid the issue

(prefer-coding-system 'utf-8-unix)

(when (< emacs-major-version 26)
  ;; apparently org switched to the function form at some point
  (defun temporary-file-directory () temporary-file-directory)
  ;; we used to be able to set this in ow.el and everything would
  ;; succeed, but it seems that at some point something change and
  ;; the 25 now waits infinitely trying to connect to github so we
  ;; never get to ow.el where this is also set, for some reason 24
  ;; does not have this issue
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(unless (featurep 'reval)
  (defvar reval-cache-directory (concat user-emacs-directory "reval/cache/"))
  (defun reval-minimal (cypher checksum path-or-url &rest alternates)
    "Simplified and compact implementation of reval."
    (let* (done (o url-handler-mode) (csn (symbol-name checksum))
           (cache-path (concat reval-cache-directory (substring csn 0 2) "/" csn
                               "-" (file-name-nondirectory path-or-url))))
      (url-handler-mode)
      (unwind-protect
          (cl-loop for path-or-url in (cons cache-path (cons path-or-url alternates))
                   do (when (file-exists-p path-or-url)
                        (let* ((buffer (find-file-noselect path-or-url))
                               (buffer-checksum (intern (secure-hash cypher buffer))))
                          (if (eq buffer-checksum checksum)
                              (progn
                                (unless (string= path-or-url cache-path)
                                  (let ((parent-path (file-name-directory cache-path))
                                        make-backup-files)
                                    (unless (file-directory-p parent-path)
                                      (make-directory parent-path t))
                                    (with-current-buffer buffer
                                      (write-file cache-path))))
                                (eval-buffer buffer)
                                (setq done t))
                            (kill-buffer buffer) ; kill so cannot accidentally evaled
                            (error "reval: checksum mismatch! %s" path-or-url))))
                   until done)
        (unless o
          (url-handler-mode 0)))))
  (defalias 'reval #'reval-minimal)
  (reval 'sha256 '3620321396c967395913ff19ce507555acb92335b0545e4bd05ec0e673a0b33b
         "https://raw.githubusercontent.com/tgbugs/orgstrap/300b1d5518af53d76d950097bcbcd7046cfa2285/reval.el"))

(let ((ghost "https://raw.githubusercontent.com/tgbugs/orgstrap/"))
  ;; FIXME ghost breaks the reval helper code
  (unless (featurep 'ow)
    (reval 'sha256 'cc8cfa015e1f3413a5fb0587f5090602604f631fee365233e1b418d83ab4333f
           ;;"~/git/orgstrap/ow.el"
           (concat ghost "f01ec0c0601e1c03e1496e1f3cba2f2a7386bdeb" "/ow.el")))
  (reval 'sha256 'dbb7c1fc0ffc72ba13f313d60830e52da652a43338b7bfabd0013cad45d79dd8
         ;; "~/git/orgstrap/init-content.el"
         (concat ghost "86f2ab7449c6caea6a9eb7ce6c3c8af0da82dd1f" "/init-content.el")))

;;;; init-simple.el ends here
