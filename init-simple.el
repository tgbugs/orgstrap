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
;; to do this init-simple.el will only update if `reval-update' is
;; invoked explicitly. For this reason we do not include the machinery
;; for updating init-simple.el itself using reval. At some point I
;; will get caching implemented as part of reval, at which point we
;; will only have to go to network once per update.

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
  (reval 'sha256 '1eb9c155d89e5d1f14b3f62f6acdeb6ab03647662ff9473ad211c9e4e5e297ef
         ;; "~/git/orgstrap/reval.el"
         "https://raw.githubusercontent.com/tgbugs/orgstrap/2ee5438e6382786d1269310cef0315eca0d227be/reval.el"))

(let ((ghost "https://raw.githubusercontent.com/tgbugs/orgstrap/"))
  ;; FIXME ghost breaks the reval helper code
  (unless (featurep 'ow)
    (reval 'sha256 '68c873965d1f054dea88e602ccf19676efbb65b16f7aefbe444f7688845ca99c ; FIXME somehow this is broken on 25 ??
           ;; "~/git/orgstrap/ow.el"
           (concat ghost "2ee5438e6382786d1269310cef0315eca0d227be" "/ow.el")))
  (reval 'sha256 '60d5c137be54270c7a05ab5b21c018d08774d803e780e68589c65d44c0fd8d11
         ;; "~/git/orgstrap/init-content.el"
         (concat ghost "f889e6a3de2ef16f8ba66a9b27762ed3325e1b8a" "/init-content.el")))


;;;; init-simple.el ends here
