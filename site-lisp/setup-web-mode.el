;;; package --- web-mode
;;;
;;; Commentary:
;;; Web mode related
;;;
;;; Code:
(require 'use-package)

(use-package web-mode
  :ensure t
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

          (setq web-mode-engines-alist
                '(("php"    . "\\.phtml\\'")))
          (setq web-mode-ac-sources-alist
                '(("css" . (ac-source-css-property))
                  ("html" . (ac-source-words-in-buffer ac-source-abbrev)))))
  :config (progn
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (setq web-mode-snippets nil)
            (define-key web-mode-map (kbd "C-c C-s") nil)
            (define-key web-mode-map (kbd "C-c C-d C-d") 'ng-snip-show-docs-at-point)

            ;; React
            (flycheck-add-mode 'javascript-eslint 'web-mode)
            ;;(sp-local-pair 'web-mode "<" ">")
            ))

(use-package emmet-mode :ensure t
  :init (progn
          (add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
          (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
          ))

(use-package helm-emmet :ensure t)

(provide 'setup-web-mode)
;;; setup-web-mode.el ends here
