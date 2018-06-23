;;; packages.el --- minimal packages needed for html export

;;; Commentary:
;; 

(setq use-package-always-ensure t)

;; * org-mode
(use-package org
             :mode ("\\.org\\'" . org-mode)
             :config
             (defun org-custom-link-img-follow (path)
               (org-open-link-from-string path))
             (defun org-custom-link-img-export (path desc format)
               (cond ((eq format 'html)
                      (format "<a href=\"%s\"><img src=\"%s\"/></a>" desc path))))
             ; TODO figure out how to suppress the message here
             (org-add-link-type "img" 'org-custom-link-img-follow 'org-custom-link-img-export))

(use-package htmlize)

(provide 'packages)

;;; packages.el ends here
