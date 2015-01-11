;;; package --- CSharp
;;;
;;; Commentary:
;;; C# and related
;;;
;;; Code:
(require 'use-package)

(use-package csharp-mode
  :ensure t
  :init (add-hook 'csharp-mode-hook 'omnisharp-mode))

(use-package omnisharp
  :ensure t
  :config (setq omnisharp-server-executable-path "/Users/frode/git/dotnet/OmniSharpServer/OmniSharp/bin/Debug/OmniSharp.exe"))

(provide 'setup-csharp-mode)
;;; setup-csharp-mode.el ends here
