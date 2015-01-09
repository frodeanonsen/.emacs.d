(require 'yasnippet)

(yas-global-mode 1)

(eval-after-load "sgml-mode"
  '(define-key html-mode-map (kbd "C-c C-d") 'ng-snip-show-docs-at-point))

(provide 'setup-snippets)
