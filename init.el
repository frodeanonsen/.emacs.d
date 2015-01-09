;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(message "init")

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(setq 3rd-party-dir
      (expand-file-name "3rd-party" site-lisp-dir))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path 3rd-party-dir)

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
     ;;dash
     magit
     projectile
     clojure-mode
     web-mode
     js2-mode
     js2-refactor
     json-mode
     scss-mode
     omnisharp
     cider
     auto-complete
     ac-cider
     fill-column-indicator
     ;;ido-ubiquitous
     ;;flx-ido
     ;;ido-vertical-mode
     highlight-symbol
     expand-region
     smartparens
     rainbow-delimiters
     arduino-mode
     powerline
     helm
     paradox)))

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
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'org '(require 'setup-org))

(eval-after-load 'hippie (require 'setup-hippie))
(eval-after-load 'smart (require 'setup-smartparens))
(eval-after-load 'protile  (require 'setup-projectile))
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

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; Misc
(when is-mac (require 'mac))
(when is-win (require 'win))

;; Lorem Ipsum generator
(require 'lorem-ipsum)

;; Email, baby
(require 'setup-mu4e)
(require 'setup-email)

;; Web mode
(require 'setup-web-mode)

;; Js mode
(require 'setup-js-mode)

;; Sass mode
(require 'setup-sass-mode)

;; CSharp mode
(require 'setup-csharp-mode)

(require 'setup-snippets)

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
