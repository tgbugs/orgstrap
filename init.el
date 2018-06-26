;;; init.el --- entry point
;;; Commentary:
;; minimal init to bootstrap orgmode
;; for compiling documentation in CI
;; adapted from scimax/init.el

(setq gc-cons-threshold 80000000)

(defconst working-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory where this file is located")

(setq package-user-dir (expand-file-name "elpa"  working-dir))

(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)

  (add-to-list
   'package-archives
   (cons "org" (concat proto "://orgmode.org/elpa/")))

  (when no-ssl
    (setq package-check-signature nil)
    (setq tls-program
  	  ;; Defaults:
  	  '("gnutls-cli --insecure -p %p %h"
  	    "gnutls-cli --insecure -p %p %h --protocols ssl3"
  	    "openssl s_client -connect %h:%p -no_ssl2 -ign_eof")
  	  ;; '("gnutls-cli -p %p %h"
  	  ;;   "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")
  	  ))

  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(add-to-list 'load-path working-dir)

(set-language-environment "UTF-8")

(require 'bootstrap)
(require 'packages)

(defun compile-org-file ()
  ; from https://joelmccracken.github.io/entries/reading-writing-data-in-emacs-batch-via-stdin-stdout/
  ; NOTE writing a comile-org-forever doesn't seem to work because read-from-minibuffer cannot block
  ; NOTE if you wrap emacs for launch in any way e.g. with emacs "${@}" & this script will fail
  ; NOTE in emacs 25 the title of the buffer takes precedence for some reason?
  (interactive)
  (let ((org-document-content "")
        this-read)
    (while (setq this-read (ignore-errors
                             (read-from-minibuffer "")))
      (setq org-document-content (concat org-document-content "\n" this-read)))

    (with-temp-buffer
      (org-mode)
      (insert org-document-content)
      (org-html-export-as-html)
      (princ (buffer-string)))))

(provide 'init)

;;; init.el ends here
