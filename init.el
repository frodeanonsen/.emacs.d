;;; package --- My Emacs configuration
;;;
;;; Commentary:
;;; Kick off the Emacs configuration here. Details in included files.
;;;
;;; Code:

;; Set path to dependencies
(defvar site-lisp-dir
  (expand-file-name "site-lisp" user-emacs-directory))

;;(defvar 3rd-party-dir
;;  (expand-file-name "3rd-party" site-lisp-dir))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)
;;(add-to-list 'load-path 3rd-party-dir)

(require 'setup-gui)
(require 'appearance)
(require 'setup-emacs)

;; Add external projects to load path
;;(dolist (project (directory-files 3rd-party-dir t "\\w+"))
;;  (when (file-directory-p project)
;;    (add-to-list 'load-path project)))

;; Setup packages
(require 'setup-package)
(require 'use-package)
(use-package s)
(use-package dash)

(require 'os-detection)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(
     auto-complete
     clojure-mode
     cider
     ac-cider
     web-mode
     js2-mode
     js2-refactor
     json-mode
     scss-mode
     omnisharp
     fill-column-indicator
     highlight-symbol
     expand-region
     smartparens
     rainbow-delimiters
     arduino-mode
     powerline
     helm
     helm-swoop
     ace-jump-mode
     ace-isearch
     paradox
     flycheck
     flycheck-pos-tip
     flycheck-clojure)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup environment variables from the user's shell.
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Setup extensions
(require 'setup-magit)
(require 'setup-projectile)
(require 'setup-ido)

(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'hippie (require 'setup-hippie))
(eval-after-load 'smart (require 'setup-smartparens))
(eval-after-load 'ac (require 'setup-auto-complete))
(eval-after-load 'hilite (require 'highlight-symbol))
(eval-after-load 'power (require 'setup-powerline))
(eval-after-load 'java (require 'setup-java))
(eval-after-load 'paradox-packages (require 'setup-paradox))

;; Language specific setup files
(eval-after-load 'clojure-mode '(require 'setup-clojure-mode))

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'setup-editor)

;; Lorem Ipsum generator
(require 'lorem-ipsum)

;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;; Web mode
(require 'setup-web-mode)

;; Js mode
(require 'setup-js-mode)

;; Sass mode
(require 'setup-sass-mode)

;; CSharp mode
(require 'setup-csharp-mode)

(require 'setup-snippets)

(require 'ace-isearch)
(global-ace-isearch-mode +1)


;; Monday ftw
(set-variable 'calendar-week-start-day 1)

;; Set default dir to my git dir
(setq default-directory "~/git")

;; Setup Lisp mode
(defun lisp-mode-setup-hooks ()
  (eldoc-mode))

(add-hook 'emacs-lisp-mode-hook 'lisp-mode-setup-hooks)

;; Rainbow delimiters all the way
(require 'setup-rainbow-delimiters)

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Setup key bindings and helm - last, to override stuff
(require 'key-bindings)
(eval-after-load 'helm-setup (require 'setup-helm))

;;; init.el ends here
