;;; init --- My Emacs configuration
;;;
;;; Commentary:
;;; Kick off the Emacs configuration here.  Details in included files.
;;;
;;; Code:

;; Load secrets
(load "~/.emacs.secrets" t)

;; Set path to dependencies
(defvar site-lisp-dir
  (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)

(require 'setup-gui)
(require 'setup-emacs)
(require 'setup-package)
(require 'appearance)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package s :ensure t)
(use-package dash :ensure t)
(message "pre os detect")
;;(require 'os-detection)
(setq is-mac 't)
(setq is-win nil)

(require 'mac)
(message "post os detect")
(use-package zoom-frm :ensure t)
(use-package expand-region :ensure t)
(use-package rainbow-delimiters
  :ensure t
  :init (progn
          (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
          (add-hook 'csharp-mode-hook #'rainbow-delimiters-mode)))

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup environment variables from the user's shell.
(use-package exec-path-from-shell
  :if is-mac
  :ensure t
  :config (exec-path-from-shell-initialize))

;; Tool to launch OSX finder
(use-package reveal-in-osx-finder
  :if is-mac
  :ensure t
  :config (global-set-key (kbd "C-c z") 'reveal-in-osx-finder))

;; Setup extensions
(use-package diminish :ensure t)
(require 'setup-magit)
(require 'setup-projectile)
(require 'setup-ido)
(require 'setup-dired)
(require 'setup-auto-complete)
(require 'setup-org)
(require 'setup-hippie)
(require 'setup-smartparens)
(use-package powerline
  :ensure t
  :config (powerline-default-theme))
(use-package paradox
  :ensure t
  :config (setq paradox-github-token my-paradox-github-token))

;; Language specific setup files
(require 'setup-clojure-mode)
(use-package javadoc-lookup :ensure t)
(require 'setup-web-mode)
(require 'setup-js-mode)
(use-package scss-mode
  :ensure t
  :config (progn
            (setq scss-compile-at-save nil)
            (setq css-indent-offset 2)
            ;;(define-key scss-mode-map (kbd "C-c C-i") 'indent-buffer)
            ))
(require 'setup-csharp-mode)
(use-package arduino-mode :ensure t)
(use-package scad
  :ensure scad-mode
  :defer t)
(use-package yaml-mode :ensure t)
(use-package highlight-symbol :ensure t)
(use-package visual-regexp :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package apache-mode :ensure t)
(require 'setup-java)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'setup-editor)
(use-package lorem-ipsum :ensure t)
(require 'setup-flycheck)
(require 'setup-snippets)

;; (use-package ace-isearch
;;   :ensure t
;;   :config (global-ace-isearch-mode +1))

;; Setup key bindings and helm - last, to override stuff
(require 'key-bindings)
(require 'setup-helm)

;;; init.el ends here
(put 'downcase-region 'disabled nil)
