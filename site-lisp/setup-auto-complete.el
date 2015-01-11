;;; package --- autocomplete
;;;
;;; Commentary:
;;; Set up autocomplete stuff
;;;
;;; Code:
(require 'use-package)

(use-package auto-complete
  :ensure t
  :init (progn
          (setq ac-show-menu-immediately-on-auto-complete t)
          (ac-config-default)))

(use-package ac-helm :ensure t)

(provide 'setup-auto-complete)
;;; setup-auto-complete.el ends here
