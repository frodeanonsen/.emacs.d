(require 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'csharp-mode-hook #'rainbow-delimiters-mode)

(provide 'setup-rainbow-delimiters)
