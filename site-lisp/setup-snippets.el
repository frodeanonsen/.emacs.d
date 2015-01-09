(require 'yasnippet)
(require 'angular-snippets)

(yas-global-mode 1)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(eval-after-load "web-mode"
  '(define-key web-mode-map (kbd "C-c C-d C-d") 'ng-snip-show-docs-at-point))

(provide 'setup-snippets)
