(require 'dash)

;; change command to meta, and ignore option to use weird Norwegian keyboard
;; (setq mac-option-modifier 'super)
;; (setq mac-command-modifier 'meta)
;; (setq ns-function-modifier 'hyper)

(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key

(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super) ; Right Windows key

(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper) ; Menu/App key

(provide 'win)
