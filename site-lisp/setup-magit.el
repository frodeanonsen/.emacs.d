;;; setup-magit.el --- Setup magit
;;;
;;; Commentary:
;;; Install and configure magit and related packages
;;;
;;; Code:

(require 'use-package)

(defun magit-save-and-exit-commit-mode ()
  (interactive)
  (save-buffer)
  (server-edit)
  (delete-window))

(defun magit-exit-commit-mode ()
  (interactive)
  (kill-buffer)
  (delete-window))

;; C-c C-a to amend without any prompt
(defun magit-just-amend ()
  (interactive)
  (save-window-excursion
    (magit-with-refresh
     (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))

;; C-x C-k to kill file on line
(defun magit-kill-file-on-line ()
  "Show file on current magit line and prompt for deletion."
  (interactive)
  (magit-visit-item)
  (delete-current-buffer-file)
  (magit-refresh))

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

;; full screen vc-annotate
(defun vc-annotate-quit ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

;; ignore whitespace
(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

;; Don't bother me with flyspell keybindings
;; (eval-after-load "flyspell"
;;   '(define-key flyspell-mode-map (kbd "C-.") nil))

(use-package magit  
  :ensure t
  :init (progn
          ;; Make sure we use latest git
          (when is-win (setq magit-git-executable "~/../../../../Program Files (x86)/Git/bin/git"))
          (when is-mac (setq magit-git-executable "/usr/local/bin/git")))
  :config (progn
            ;; Non-global keybindings
            (bind-key "C-c C-a" 'magit-just-amend magit-status-mode-map)
            (bind-key "C-x C-k" 'magit-kill-file-on-line magit-status-mode-map)
            (bind-key "q" 'magit-quit-session magit-status-mode-map)
            (bind-key "W" 'magit-toggle-whitespace magit-status-mode-map)
            
            ;; Load git configurations
            ;; For instance, to run magit-svn-mode in a project, do:
            ;;
            ;;     git config --add magit.extension svn
            ;;
            (add-hook 'magit-mode-hook 'magit-load-config-extensions)

            ;; Subtler highlight
            (set-face-background 'diff-file-header "#121212")
            (set-face-foreground 'diff-context "#666666")
            (set-face-foreground 'diff-added "#00cc33")
            (set-face-foreground 'diff-removed "#ff0000")

            (set-default 'magit-stage-all-confirm nil)
            (set-default 'magit-unstage-all-confirm nil)))

(eval-after-load "vc-annotate"
  '(progn
     (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))

     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)
     (message "vc-annotate loaded")))

(use-package ediff
  :ensure t
  :config (progn
            (set-face-foreground 'ediff-odd-diff-B "#ffffff")
            (set-face-background 'ediff-odd-diff-B "#292521")
            (set-face-foreground 'ediff-even-diff-B "#ffffff")
            (set-face-background 'ediff-even-diff-B "#292527")

            (set-face-foreground 'ediff-odd-diff-A "#ffffff")
            (set-face-background 'ediff-odd-diff-A "#292521")
            (set-face-foreground 'ediff-even-diff-A "#ffffff")
            (set-face-background 'ediff-even-diff-A "#292527")))

(use-package git-messenger
  :ensure t
  ;; Show blame for current line
  :config (global-set-key (kbd "C-x v p") #'git-messenger:popup-message))

(use-package git-timemachine :ensure t)

(use-package magit-gitflow
  :ensure t
  :config (progn
            (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)))

(provide 'setup-magit)
;;; setup-magit.el ends here
