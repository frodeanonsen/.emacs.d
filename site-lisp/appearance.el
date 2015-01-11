;;; package --- appearance
;;;
;;; Commentary:
;;; Set up themes
;;;
;;; Code:

(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Highlight current line
(global-hl-line-mode 1)

;; Set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "themes"))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

;; Default theme
(defun use-presentation-theme ()
  (interactive)
  (disable-theme 'ample-zen)
  (load-theme 'prez t)
  (when (boundp 'magnars/presentation-font)
    (set-face-attribute 'default nil :font magnars/presentation-font)))

(defun use-default-theme ()
  (interactive)
  (disable-theme 'prez)
  (load-theme 'ample-zen t)
  (when (boundp 'magnars/default-font)
    (set-face-attribute 'default nil :font magnars/default-font)))

(defun toggle-presentation-mode ()
  (interactive)
  (if (string= (frame-parameter nil 'font) magnars/default-font)
      (use-presentation-theme)
    (use-default-theme)))

(global-set-key (kbd "C-<f9>") 'toggle-presentation-mode)

(use-default-theme)

;; Customize Smartparens pair-match highlight color
(custom-set-faces
 '(sp-show-pair-match-face ((t (:background "dark red")))))

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; org-mode colors
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Make zooming affect frame instead of buffers
(use-package zoom-frm :ensure t)

(provide 'appearance)
;;; appearance.el ends here
