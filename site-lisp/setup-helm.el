(require 'helm)

(require 'helm-projectile)
(require 'helm-config)
(require 'helm-descbinds)
(helm-descbinds-mode 1)
(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c pf") 'helm-browse-project)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x f") 'helm-recentf)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "s-p") 'helm-projectile-switch-project)
(global-set-key (kbd "s-f") 'helm-projectile-find-file)

(provide 'setup-helm)
