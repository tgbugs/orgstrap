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
  (defconst working-dir (file-name-directory (or load-file-name (buffer-file-name)))
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
  (reval 'sha256 'f978168b5c0fc0ce43f69c748847e693acc545df9a3ff1d9def57bdb1fc63c4a
         ;; "~/git/orgstrap/reval.el"
         "https://raw.githubusercontent.com/tgbugs/orgstrap/649fd0cdcb831dcd840c66ee324005165ce970ca/reval.el"))

(let ((ghost "https://raw.githubusercontent.com/tgbugs/orgstrap/"))
  ;; FIXME ghost breaks the reval helper code
  (unless (featurep 'ow)
    (reval 'sha256 'a90b12c386d60882cadeb6b6557f7eb05378bfcf94f68f7f8512a9edfeb34d6c
           ;; "~/git/orgstrap/ow.el"
           (concat ghost "98350dc97b6a079d35c94b1798501a62cbbdf176" "/ow.el")))
  (reval 'sha256 '715f6d3ca6cb3ce91185af85dcb4a307dbf2912f18a9c36f8ea76faf1772b4b4
         ;; "~/git/orgstrap/init-content.el"
         (concat ghost "7111b8c4dab5a5404bb06e816abf35df96caa739" "/init-content.el")))

;;;; init-simple.el ends here
