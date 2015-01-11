;;; package --- Setup magit
;;;
;;; Commentary:
;;; Interactively Do Things
;;;
;;; Code:

(require 'use-package)
(require 'dash)

(defun my/ido-go-straight-home ()
  (interactive)
  (cond
   ((looking-back "~/") (insert "projects/"))
   ((looking-back "/") (insert "~/"))
   (:else (call-interactively 'self-insert-command))))

(use-package ido
  :ensure t
  :config (progn
            (ido-mode t)
            (setq ido-enable-prefix nil
                  ido-enable-flex-matching t
                  ido-case-fold nil
                  ido-auto-merge-work-directories-length -1
                  ido-create-new-buffer 'always
                  ido-use-filename-at-point nil
                  ido-max-prospects 10)

            ;;(bind-key "C-n" 'ido-next-match ido-completion-map)
            ;;(bind-key "<down>" 'ido-next-match ido-completion-map)
            ;;(bind-key "C-p" 'ido-prev-match ido-completion-map)
            ;;(bind-key "<up>" 'ido-prev-match ido-completion-map)

            (bind-key "~" 'my/ido-go-straight-home ido-file-completion-map)
            (bind-key "C-~" 'my/ido-go-straight-home ido-file-completion-map)
            (bind-key "C-w" 'ido-delete-backward-updir ido-file-completion-map)
            (bind-key "C-x C-w" 'ido-copy-current-file-name ido-file-completion-map)
            (bind-key "C-w" 'ido-delete-backward-updir ido-file-dir-completion-map)
            (bind-key "C-x C-w" 'ido-copy-current-file-name ido-file-dir-completion-map)

            ;; Always rescan buffer for imenu
            (set-default 'imenu-auto-rescan t)

            (add-to-list 'ido-ignore-directories "target")
            (add-to-list 'ido-ignore-directories "node_modules")))

;; Try out flx-ido for better flex matching between words
(use-package flx-ido
  :ensure t
  :config (progn
            (flx-ido-mode 1)

            ;; disable ido faces to see flx highlights.
            (setq ido-use-faces nil)))

;; flx-ido looks better with ido-vertical-mode
(use-package ido-vertical-mode
  :ensure t
  :config (ido-vertical-mode))

;; Ido at point (C-,)
(use-package ido-at-point
  :ensure t
  :bind (("C-," . completion-at-point))
  :config (ido-at-point-mode))


;; Use ido everywhere

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(use-package ido-ubiquitous
  :config (progn
            (ido-ubiquitous-mode 1)
            (ido-ubiquitous-use-new-completing-read webjump 'webjump)
            (ido-ubiquitous-use-new-completing-read yas-expand 'yasnippet)
            (ido-ubiquitous-use-new-completing-read yas-visit-snippet-file 'yasnippet)))

(provide 'setup-ido)
;;; setup-ido.el ends here
