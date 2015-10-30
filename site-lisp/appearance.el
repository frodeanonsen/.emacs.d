;;; appearance.el --- appearance
;;;
;;; Commentary:
;;; Set up themes. Runs BEFORE package-setup.el, so cannot use-packacge
;;;
;;; Code:
(require 'use-package)

(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Highlight current line
(global-hl-line-mode 1)

;; (use-package ample-theme
;;   :init (progn (load-theme 'ample t t)
;;                (load-theme 'ample-flat t t)
;;                (enable-theme 'ample-flat))
;;   :defer t
;;   :ensure t)

(use-package ample-zen-theme
  :init (load-theme 'ample-zen t)
  :defer t
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :defer t
  :ensure t)

;; Set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "themes"))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

;; mac friendly font
(when window-system
  (setq frode/default-font "-apple-Monaco-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1")
  (setq frode/presentation-font "-apple-Monaco-medium-normal-normal-*-21-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font frode/default-font))

;; Default theme
(defun use-presentation-theme ()
  (setq current-theme "prez")
  (interactive)  
  (load-theme 'sanityinc-tomorrow-day t)
  (set-face-background hl-line-face "#eeeeee")

  ;; Customize Smartparens pair-match highlight color
  (custom-set-faces
   '(sp-show-pair-match-face ((t (:background "light red")))))

  (when (boundp 'frode/default-font)
    (set-face-attribute 'default nil :font frode/default-font)))

(defun use-default-theme ()
  (setq current-theme "default")
  (interactive)
  (enable-theme 'ample-zen)

  ;; Customize Smartparens pair-match highlight color
  (custom-set-faces
   '(sp-show-pair-match-face ((t (:background "dark red")))))

  (when (boundp 'frode/default-font)
    (set-face-attribute 'default nil :font frode/default-font)))

(defun toggle-presentation-mode ()
  (interactive)
  (if (string= current-theme "default")
      (use-presentation-theme)
    (use-default-theme)))

(global-set-key (kbd "C-<f9>") 'toggle-presentation-mode)

(use-default-theme)

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; org-mode colors
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "light blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("SOMEDAY" :foreground "forest green" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(provide 'appearance)
;;; appearance.el ends here
