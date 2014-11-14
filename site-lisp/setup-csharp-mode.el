(require 'csharp-mode)
(require 'omnisharp)

(add-hook 'csharp-mode-hook 'omnisharp-mode)

(setq omnisharp-server-executable-path "/Users/frode/git/dotnet/OmniSharpServer/OmniSharp/bin/Debug/OmniSharp.exe")

(provide 'setup-csharp-mode)
