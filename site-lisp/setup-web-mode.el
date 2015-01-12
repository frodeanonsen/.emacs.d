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

          (setq web-mode-engines-alist
                '(("php"    . "\\.phtml\\'")))
          (setq web-mode-ac-sources-alist
                '(("css" . (ac-source-css-property))
                  ("html" . (ac-source-words-in-buffer ac-source-abbrev)))))
  :config (progn
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-snippets nil)
            (define-key web-mode-map (kbd "C-c C-s") nil)
            (define-key web-mode-map (kbd "C-c C-d C-d") 'ng-snip-show-docs-at-point)))

(use-package zencoding-mode
  :ensure t
  :init (progn
          (add-hook 'sgml-mode-hook 'zencoding-mode)
          (add-hook 'web-mode-hook 'zencoding-mode))
  :config (progn
            (define-key zencoding-mode-keymap (kbd "C-j") 'zencoding-expand-line)
            (define-key zencoding-mode-keymap (kbd "<C-return>") nil)))

(provide 'setup-web-mode)
;;; setup-web-mode.el ends here
