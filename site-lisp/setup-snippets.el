(require 'yasnippet)

(yas-global-mode 1)

(eval-after-load "web-mode"
  '(define-key web-mode-map (kbd "C-c C-d C-d") 'ng-snip-show-docs-at-point))

(provide 'setup-snippets)
