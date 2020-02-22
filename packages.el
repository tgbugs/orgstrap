;;; packages.el --- minimal packages needed for html export

;;; Commentary:
;; 

(setq use-package-always-ensure t)

;; * org-mode
(use-package org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-export-with-broken-links t "needed for hrefl among others")
  (org-src-fontify-natively t)
  :config

  ;; img tag links
  ;; NOTE these are for nested images inside other links NOT for images
  (defun org-custom-link-img (path)
    (org-link-open-from-string path))
  (defun org-custom-link-img-export (path desc format)
    (cond ((eq format 'html)
           (format "<a href=\"%s\"><img src=\"%s\"/></a>" desc path))))

  ;; TODO figure out how to suppress the message here
  (org-link-set-parameters "img"
                           :follow #'org-custom-link-img
                           :export #'org-custom-link-img-export)

  ;; server local hrefs
  (defun org-custom-hrefl (path)
    "do nothing because we don't know the context of the export")
  (defun org-custom-hrefl-export (path desc format)
    "export a rooted path as an absolute href with no server"
    (cond ((eq format 'html)
           (format "<a href=\"%s\">%s</a>" path desc))))
  (org-link-set-parameters "hrefl"
                           :follow #'org-custom-hrefl
                           :export #'org-custom-hrefl-export))

(use-package htmlize)

(provide 'packages)

;;; packages.el ends here
