;;; package --- smartparens
;;;
;;; Commentary:
;;; Smartparens config and bindings
;;;
;;; Code:
(require 'use-package)

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init (progn
          (add-hook 'smartparens-enabled-hook 'smartparens-strict-mode)
          (bind-key "s-<right>" 'sp-forward-slurp-sexp smartparens-mode-map)
          (bind-key "s-<left>" 'sp-forward-barf-sexp smartparens-mode-map))
  :config (progn
            (require 'smartparens-config)
            (smartparens-global-mode t)
            (show-smartparens-global-mode t)

            (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
            (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

            (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
            (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
            (define-key smartparens-mode-map (kbd "C-S-a") 'sp-beginning-of-sexp)
            (define-key smartparens-mode-map (kbd "C-S-d") 'sp-end-of-sexp)

            (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
            (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
            (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
            (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

            (define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
            (define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

            (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
            (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

            (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
            (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

            (define-key smartparens-mode-map (kbd "s-S-<left>") 'sp-backward-slurp-sexp)
            (define-key smartparens-mode-map (kbd "s-S-<right>") 'sp-backward-barf-sexp)

            (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
            (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
            (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
            (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

            (define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
            (define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
            (define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

            (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
            (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

            (define-key smartparens-mode-map (kbd "H-t") 'sp-prefix-tag-object)
            (define-key smartparens-mode-map (kbd "H-p") 'sp-prefix-pair-object)
            (define-key smartparens-mode-map (kbd "H-s c") 'sp-convolute-sexp)
            (define-key smartparens-mode-map (kbd "H-s a") 'sp-absorb-sexp)
            (define-key smartparens-mode-map (kbd "H-s e") 'sp-emit-sexp)
            (define-key smartparens-mode-map (kbd "H-s p") 'sp-add-to-previous-sexp)
            (define-key smartparens-mode-map (kbd "H-s n") 'sp-add-to-next-sexp)
            (define-key smartparens-mode-map (kbd "H-s j") 'sp-join-sexp)
            (define-key smartparens-mode-map (kbd "H-s s") 'sp-split-sexp)

            ;; pair management
            (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

            ;; markdown-mode
            (sp-with-modes '(markdown-mode gfm-mode rst-mode)
              (sp-local-pair "*" "*" :bind "C-*")
              (sp-local-tag "2" "**" "**")
              (sp-local-tag "s" "```scheme" "```")
              (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

            ;; tex-mode latex-mode
            (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
              (sp-local-tag "i" "\"<" "\">"))

            ;; html-mode
            (sp-with-modes '(web-mode html-mode sgml-mode)
              (sp-local-pair "<" ">"))

            ;; lisp modes
            (sp-with-modes sp--lisp-modes
              (sp-local-pair "(" nil :bind "C-("))

            ;; More keybindings
            (define-key smartparens-mode-map (kbd "M-r") 'raise-sexp)
            (define-key smartparens-mode-map (kbd "M-q") 'sp-indent-defun)

            (sp-pair "(" ")" :wrap "M-(")))

(provide 'setup-smartparens)
;;; setup-smartparens.el ends here
