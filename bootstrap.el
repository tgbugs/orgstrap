;;; bootstrap.el --- install use-package
;;; Commentary:
;; adapted from scimax/bootstrap.el

;;; Code:
(unless package--initialized (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(provide 'bootstrap)

;;; bootstrap.el ends here
