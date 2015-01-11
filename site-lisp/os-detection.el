;;; package --- Setup os-detection
;;;
;;; Commentary:
;;; Configure different os-specific variables
;;;
;;; Code:

(setq is-mac (equal system-type 'darwin))
(setq is-win (equal system-type 'windows-nt))

(when is-mac (require 'mac))
(when is-win (require 'win))

(provide 'os-detection)
;;; os-detection.el ends here
