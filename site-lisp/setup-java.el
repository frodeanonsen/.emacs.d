;;; package --- java
;;;
;;; Commentary:
;;; Java stuff
;;;
;;; Code:
(require 'use-package)

;; (use-package emacs-eclim
;;   :ensure t
;;   :config (progn
;;             (global-eclim-mode)
;;             (ac-emacs-eclim-config)
;;             (custom-set-variables
;;              '(eclim-eclipse-dirs '("/Applications/Eclipse.app/Contents/Eclipse"))
;;              '(eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/eclim"))
;;             (setq java-basic-offset 2
;;                   tab-width 2
;;                   indent-tabs-mode nil)))

;; (require 'eclim)
;; (require 'eclimd)
;; (require 'ac-emacs-eclim-source)

(use-package groovy-mode :ensure t)

(provide 'setup-java)
;;; setup-java.el ends here
