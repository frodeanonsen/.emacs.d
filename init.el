;;; package --- My Emacs configuration
;;;
;;; Commentary:
;;; Kick off the Emacs configuration here. Details in included files.
;;;
;;; Code:

;; Set path to dependencies
(defvar site-lisp-dir
  (expand-file-name "site-lisp" user-emacs-directory))

(defvar 3rd-party-dir
  (expand-file-name "3rd-party" site-lisp-dir))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path 3rd-party-dir)

(require 'setup-gui)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Set up appearance early
(require 'appearance)

;; Add external projects to load path
(dolist (project (directory-files 3rd-party-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; OS detection
(setq is-mac (equal system-type 'darwin))
(setq is-win (equal system-type 'windows-nt))

;; Setup packages
(require 'setup-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(
     ;;;;;;;dash
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


(eval-after-load 'ido '(require 'setup-ido))
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

(require 'expand-region)
(require 'multiple-cursors)
(require 'smart-forward)


;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "#111122")

;; Misc
(when is-mac (require 'mac))
(when is-win (require 'win))

;; Lorem Ipsum generator
(require 'lorem-ipsum)

;; Email, baby
(require 'setup-mu4e)
(require 'setup-email)

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
