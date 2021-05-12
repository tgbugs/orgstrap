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
  (defun reval-minimal (cypher checksum path-or-url &rest _alts)
    (let ((o url-handler-mode))
      (url-handler-mode)
      (unwind-protect
          (when (file-exists-p path-or-url)
            (let* ((buffer (find-file-noselect path-or-url))
                   (buffer-checksum (intern (secure-hash cypher buffer))))
              (if (eq buffer-checksum checksum)
                  (eval-buffer buffer)
                (kill-buffer buffer)
                (error "reval: checksum mismatch! %s" path-or-url))))
        (unless o
          (url-handler-mode 0)))))
  (defalias 'reval #'reval-minimal))

(let ((ghost "https://raw.githubusercontent.com/tgbugs/orgstrap/"))
  ;; FIXME ghost breaks the reval helper code
  (unless (featurep 'ow)
    (reval 'sha256 '586295a1a0f3722370570d229a84d704460eb4e819448a725780448d95a29a73
           (concat ghost "97fd4f64a7c54572e405ce3084efc0456a28c4d2" "/ow.el")))
  (reval 'sha256 'cdf6f4ea37efb50ba75033fd3b0f44e2f99e04e6bd47ba6df2880e8633ac3500
         (concat ghost "c987f7c38a6080018b414c0d866a2ccaf28afa8c" "/init-content.el")))

;;;; init-simple.el ends here
