;;; package --- Setup projectile
;;;
;;; Commentary:
;;; Install and configure projectile package
;;;
;;; Code:

(require 'use-package)

(use-package projectile
  :ensure t
  ;;:idle (projectile-global-mode)
  :init (progn
          (setq projectile-switch-project-action 'projectile-dired)
          (setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name))))
          (projectile-global-mode)))


(provide 'setup-projectile)
