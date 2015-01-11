;;; package --- helm
;;;
;;; Commentary:
;;; Helm stuff
;;;
;;; Code:
(require 'use-package)

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-c pf" . helm-browse-project)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list)
         ("s-p" . helm-projectile-switch-project)
         ("s-f" . helm-projectile-find-file))
  :config (progn
            (helm-descbinds-mode 1)
            (helm-mode 1)))

(require 'helm-config)

(use-package helm-projectile :ensure t)
(use-package helm-descbinds :ensure t)
(use-package helm-swoop :ensure t)

(provide 'setup-helm)
;;; setup-helm.el ends here
