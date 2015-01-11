;;; package --- setup-dired
;;;
;;; Commentary:
;;; Set up dired
;;;
;;; Code:
(require 'use-package)
(require 'dash)

;; C-a is nicer in dired if it moves back to start of files
(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

;; M-up is nicer in dired if it moves to the fourth line - the first file
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

;; M-down is nicer in dired if it moves to the last file
(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(use-package dired
  ;; Reload dired after making changes
  :init (progn
          (--each '(dired-do-rename
                    dired-do-copy
                    dired-create-directory
                    wdired-abort-changes)
            (eval `(defadvice ,it (after revert-buffer activate)
                     (revert-buffer))))
          (eval-after-load "wdired"
            '(progn
               (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
               (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
               (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom))))
  :config (progn
            (define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
            (define-key dired-mode-map (kbd "k") 'dired-do-delete)
            (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
            (define-key dired-mode-map (vector 'remap 'smart-up) 'dired-back-to-top)
            (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
            (define-key dired-mode-map (vector 'remap 'smart-down) 'dired-jump-to-bottom)

            ;; Jump from file to containing directory
            (global-set-key (kbd "C-x C-j") 'dired-jump) (autoload 'dired-jump "dired")
            (global-set-key (kbd "C-x M-j") '(λ (dired-jump 1)))

            ;; Delete with C-x C-k to match file buffers and magit
            (define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)

            ;; Move files between split panes
            (setq dired-dwim-target t)))

(provide 'setup-dired)
;;; setup-dired.el ends here
