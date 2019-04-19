;;; packages.el --- minimal packages needed for html export

;;; Commentary:
;; 

(setq use-package-always-ensure t)

;; * org-mode
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-export-with-broken-links 1)  ; needed on some systems

  (defun org-custom-link-img-follow (path)
    (org-open-link-from-string path))
  (defun org-custom-link-img-export (path desc format)
    (cond ((eq format 'html)
           (format "<a href=\"%s\"><img src=\"%s\"/></a>" desc path))))

  ;; TODO figure out how to suppress the message here
  (org-add-link-type "img" 'org-custom-link-img-follow 'org-custom-link-img-export)

  (defun org-custom-hrefl (path)
    "do nothing because we don't know the context of the export")
  (defun org-custom-hrefl-export (path desc format)
    "export a rooted path as a an absolute href with no server"
    (cond ((eq format 'html)
           (format "<a href=\"%s\">%s</a>" path desc))))
  (org-add-link-type "hrefl" 'org-custom-hrefl 'org-custom-hrefl-export))

(use-package htmlize)

(provide 'packages)

;;; packages.el ends here
