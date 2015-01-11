(require 'use-package)

(use-package yasnippet
  :ensure t
  :init (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config (yas-global-mode 1))

(use-package angular-snippets :ensure t)

(provide 'setup-snippets)
