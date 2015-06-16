;;; package --- Setup emacs
;;;
;;; Commentary:
;;; Basic emacs settings
;;;
;;; Code:

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Write auto-save files to own directory
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name
                (concat user-emacs-directory "auto-save")) t)))

;; Disable lockfiles
(setq create-lockfiles nil)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Monday ftw
(set-variable 'calendar-week-start-day 1)

;; Set default dir to my git dir
(setq default-directory "~/git")

;; Setup Lisp mode
(defun lisp-mode-setup-hooks ()
  (eldoc-mode))

(add-hook 'emacs-lisp-mode-hook 'lisp-mode-setup-hooks)

(setq initial-scratch-message nil)

(setq initial-buffer-choice default-directory)

(provide 'setup-emacs)
;;; setup-emacs.el ends here
