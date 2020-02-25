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

  (add-to-list
   'package-archives
   (cons "melpa" (concat proto "://melpa.org/packages/")) t)

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

;;; batch mode color fixes
;; this one was a real adventure, found `display-color-cells' and
;; `display-color-p' and a couple other things while hunting down
;; `min-colors' in `defface' and finally landed on this
;; it really seems like this should be the default behavior in emacs

(defun noninteractive-face-spec-set-match-display (command &rest args)
  "adivse native `face-spec-set-match-display' when `noninteractive'is true
(i.e., in --batch mode) so that `org-src-fontify-natively' will work"
  t)

(when noninteractive
  (advice-add 'face-spec-set-match-display
              :around 'noninteractive-face-spec-set-match-display))

;;; use additional packages
(defmacro use-packages (&rest names)
  "enable multiple calls to `use-package' during bootstrap"
  (cons 'progn (map 'list (lambda (name) `(use-package ,name)) names)))

;;; pipe functions

(defmacro defpipefun (name &rest body)
  "define a new function for modifying things piped through std* == FUN!"
  `(defun ,name ()
     ;; from https://joelmccracken.github.io/entries/reading-writing-data-in-emacs-batch-via-stdin-stdout/
     ;; NOTE writing a comile-org-forever doesn't seem to work because read-from-minibuffer cannot block
     ;; NOTE if you wrap emacs for launch in any way e.g. with emacs "${@}" & this script will fail
     ;; NOTE in emacs 25 the title of the buffer takes precedence for some reason?
     (interactive)
     (let (this-read
           (to-be-inserted-into-buffer ""))
       (while (setq this-read (ignore-errors
                                (read-from-minibuffer "")))
         (setq to-be-inserted-into-buffer (concat to-be-inserted-into-buffer "\n" this-read)))

       (with-temp-buffer
         (insert to-be-inserted-into-buffer)
         ,@body
         (princ (buffer-string))))))

(defpipefun compile-org-file
  (org-mode)
  (org-html-export-as-html))
(defpipefun align-tables-org-file
  (org-mode)
  (org-table-map-tables 'org-table-align))
(defpipefun align-tables-and-compile-org-file
  (org-mode)
  (org-table-map-tables 'org-table-align)
  (org-html-export-as-html))

(provide 'init)

;;; init.el ends here
