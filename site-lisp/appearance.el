;;; appearance.el --- appearance
;;;
;;; Commentary:
;;; Set up themes
;;;
;;; Code:
(require 'use-package)

(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Highlight current line
(global-hl-line-mode 1)

(use-package material-theme
  :init (load-theme 'material t)
  :defer t
  :ensure t)

;; mac friendly font
(when window-system
  (setq frode/default-font "-apple-Monaco-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1")
  (setq frode/presentation-font "-apple-Monaco-medium-normal-normal-*-21-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font frode/default-font))

;; Default theme
(defun use-presentation-theme ()
  (setq current-theme "prez")
  (interactive)  
  (load-theme 'material-light t))

(defun use-default-theme ()
  (setq current-theme "default")
  (interactive)
  (enable-theme 'material))

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
