;;; packages.el --- minimal packages needed for html export

;;; Commentary:
;; 

(setq use-package-always-ensure t)

;; * org-mode
(use-package org-plus-contrib
  :mode ("\\.org\\'" . org-mode))

(use-package htmlize)

(provide 'packages)

;;; packages.el ends here
