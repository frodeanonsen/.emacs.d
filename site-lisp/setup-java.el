;;; package --- java
;;;
;;; Commentary:
;;; Java stuff
;;;
;;; Code:
(require 'use-package)

(require 'eclimd)
(require 'ac-emacs-eclim-source)

(use-package emacs-eclim
  :ensure t
  :config (progn
            (global-eclim-mode)
            (custom-set-variables
             '(eclim-eclipse-dirs '("~/apps/eclipse"))
             '(eclim-executable "~/apps/eclipse/eclim"))
            (ac-emacs-eclim-config)))

(provide 'setup-java)
;;; setup-java.el ends here
