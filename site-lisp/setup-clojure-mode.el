;;; setup-clojure-mode.el --- setup-clojure
;;;
;;; Commentary:
;;; Kick off the Emacs configuration here. Details in included files.
;;;
;;; Code:
(require 'use-package)

(defun setup-clj-refactor-mode ()
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(defun frode-cider-mode-hooks ()
  (flycheck-mode 1))

(defun frode-clojure-mode-hooks ()
  (linum-mode 1)
  (highlight-symbol-mode)
  (fci-mode)
  (auto-complete-mode)
  (setup-clj-refactor-mode)
  (flycheck-mode 1))

(defun frode-clojurescript-mode-hooks ()
  (linum-mode 1)
  (highlight-symbol-mode)
  (fci-mode)
  (setup-clj-refactor-mode))

(defun clj-hippie-expand-no-case-fold ()
  (interactive)
  (let ((old-syntax (char-to-string (char-syntax ?/))))
    (modify-syntax-entry ?/ " ")
    (hippie-expand-no-case-fold)
    (modify-syntax-entry ?/ old-syntax)))

;; Cycle between () {} []
(defun live-delete-and-extract-sexp ()
  "Delete the sexp and return it."
  (interactive)
  (let* ((begin (point)))
    (forward-sexp)
    (let* ((result (buffer-substring-no-properties begin (point))))
      (delete-region begin (point))
      result)))

(defun live-cycle-clj-coll ()
  "convert the coll at (point) from (x) -> {x} -> [x] -> (x) recur"
  (interactive)
  (let* ((original-point (point)))
    (while (and (> (point) 1)
                (not (equal "(" (buffer-substring-no-properties (point) (+ 1 (point)))))
                (not (equal "{" (buffer-substring-no-properties (point) (+ 1 (point)))))
                (not (equal "[" (buffer-substring-no-properties (point) (+ 1 (point))))))
      (backward-char))
    (cond
     ((equal "(" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "{" (substring (live-delete-and-extract-sexp) 1 -1) "}"))
     ((equal "{" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "[" (substring (live-delete-and-extract-sexp) 1 -1) "]"))
     ((equal "[" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "(" (substring (live-delete-and-extract-sexp) 1 -1) ")"))
     ((equal 1 (point))
      (message "beginning of file reached, this was probably a mistake.")))
    (goto-char original-point)))

;; Warn about missing nREPL instead of doing stupid things
(defun nrepl-warn-when-not-connected ()
  (interactive)
  (message "Oops! You're not connected to an nREPL server. Please run M-x cider or M-x cider-jack-in to connect."))

(use-package clojure-mode
  :ensure t
  :init (progn
          (defadvice clojure-test-run-tests (before save-first activate)
            (save-buffer))

          (defadvice nrepl-load-current-buffer (before save-first activate)
            (save-buffer))

          (add-hook 'clojure-mode-hook 'frode-clojure-mode-hooks)
          (add-hook 'clojurescript-mode-hook 'frode-clojurescript-mode-hooks))
  :config (progn
            (define-key clojure-mode-map (kbd "s-j") 'clj-jump-to-other-file)
            (define-key clojure-mode-map (kbd "C-.") 'clj-hippie-expand-no-case-fold)

            ;; Indent and highlight more commands
            (put-clojure-indent 'match 'defun)

            (define-key clojure-mode-map (kbd "C-´") 'live-cycle-clj-coll)
            (define-key clojure-mode-map (kbd "C-M-x")   'nrepl-warn-when-not-connected)
            (define-key clojure-mode-map (kbd "C-x C-e") 'nrepl-warn-when-not-connected)
            (define-key clojure-mode-map (kbd "C-c C-e") 'nrepl-warn-when-not-connected)
            (define-key clojure-mode-map (kbd "C-c C-l") 'nrepl-warn-when-not-connected)
            (define-key clojure-mode-map (kbd "C-c C-r") 'nrepl-warn-when-not-connected)
            (define-key clojure-mode-map (kbd "C-c C-z") 'nrepl-warn-when-not-connected)
            (define-key clojure-mode-map (kbd "C-c C-k") 'nrepl-warn-when-not-connected)
            (define-key clojure-mode-map (kbd "C-c C-n") 'nrepl-warn-when-not-connected)))

(use-package clj-refactor
  :ensure t)

(use-package cider
  :ensure t
  :init (progn
          (add-hook 'cider-mode-hook 'frode-cider-mode-hooks)

          ;; Enable eldoc in Clojure buffers
          (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))
  :config (progn
            (define-key cider-repl-mode-map (kbd "<home>") nil)
            (define-key cider-repl-mode-map (kbd "C-,") 'complete-symbol)
            (define-key cider-mode-map (kbd "C-,") 'complete-symbol)
            (define-key cider-mode-map (kbd "C-c C-q") 'nrepl-close)
            (define-key cider-mode-map (kbd "C-c C-Q") 'cider-quit)

            ;; Enable error buffer popping also in the REPL:
            (setq cider-repl-popup-stacktraces t)

            ;; Specify history file
            (setq cider-history-file "~/.emacs.d/nrepl-history")

            ;; auto-select the error buffer when it's displayed
            (setq cider-auto-select-error-buffer t)))

;; Autocomplete with tab
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

;; Autocomplete
(use-package ac-cider
  :ensure t
  :init (progn
          (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
          (add-hook 'cider-mode-hook 'ac-cider-setup)
          (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
          (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
          (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function))
  :config (progn
            (add-to-list 'ac-modes 'cider-mode)
            (add-to-list 'ac-modes 'cider-repl-mode)))

;; Flycheck
(use-package flycheck-clojure
  :ensure t
  :config (flycheck-clojure-setup))

(use-package clojure-mode-extra-font-locking :ensure t)

(provide 'setup-clojure-mode)
;;; setup-clojure-mode.el ends here
