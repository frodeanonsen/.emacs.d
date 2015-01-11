;;; package --- Setup projectile
;;;
;;; Commentary:
;;; Install and configure projectile package
;;;
;;; Code:

(require 'use-package)

(use-package projectile
  :ensure t
  :idle (projectile-global-mode)
  :init (setq projectile-switch-project-action 'projectile-dired))

(provide 'setup-projectile)
