;;; package --- js-mode
;;;
;;; Commentary:
;;; All javascript related stuff
;;;
;;; Code:
(require 'use-package)

(use-package tern :ensure t)
(use-package tern-auto-complete
  :ensure t
  :init (tern-ac-setup))

;; (use-package js3-mode
;;   :ensure t
;;   :interpreter "node"
;;   :init (progn
;;           (add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
;;           ;; Autocomplete
;;           (add-hook 'js3-mode-hook 'auto-complete-mode)
;;           (add-hook 'js3-mode-hook 'skewer-mode)
;;           ;;(add-hook 'js3-mode-hook 'ac-js2-mode)
;;           (add-hook 'js3-mode-hook 'tern-mode)
;;           (add-hook 'js3-mode-hook (lambda () (setq mode-name "J3")))
;;           ;;(setq ac-js2-evaluate-calls t)
;;           (setq js3-indent-level          2
;;                 js3-mode-dev-mode-p       t
;;                 js3-auto-indent-p         t
;;                 js3-enter-indents-newline t
;;                 js3-indent-on-enter-key   t
;;                 js3-indent-dots           t
;;                 js3-lazy-dots             t
;;                 js3-boring-indentation    t)
;;           )
;;   :config (progn
;;             (define-key js3-mode-map (kbd "M-j") nil)
;;             (setq-default js3-global-externs '("Buffer" "history" "process" "module" "exports" "require" "jQuery" "$" "_" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "describe" "it" "expect" "before" "after"))

;;             ;; 2 spaces for indent
;;             (setq-default js3-basic-offset 2)
;;             (setq-default json-reformat:indent-width 2)
;;             (setq js-indent-level 2)))

;; (defun modify-syntax-table-for-jsx ()
;;   (modify-syntax-entry ?< "(>")
;;   (modify-syntax-entry ?> ")<"))

(use-package js2-mode  
  :ensure t
  :interpreter "node"
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
          ;; Autocomplete
          (add-hook 'js2-mode-hook 'auto-complete-mode)
          (add-hook 'js2-mode-hook 'skewer-mode)
          ;; React
          (add-hook 'js2-mode-hook 'js2-imenu-extras-setup)
          ;;(add-hook 'js2-mode-hook 'modify-syntax-table-for-jsx)
          
          ;; (add-hook 'js2-mode-hook 'ac-js2-mode)
          ;; (setq ac-js2-evaluate-calls t)
          (add-hook 'js2-mode-hook 'tern-mode)
          (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))
          )
  :config (progn
            (define-key js2-mode-map (kbd "M-j") nil)
            (setq-default js2-global-externs '("Buffer" "history" "process" "module" "exports" "require" "jQuery" "$" "_" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "describe" "it" "expect" "before" "after"))

            ;; 2 spaces for indent
            (setq-default js2-basic-offset 2)
            (setq-default json-reformat:indent-width 2)
            (setq-default js-indent-level 2)
            (setq-default js2-auto-indent-p t)
            (setq-default js2-enter-indents-newline nil)
            (setq-default js2-indent-on-enter-key nil)

            ;; React
            (require 'js2-imenu-extras)
            (add-to-list 'js2-imenu-available-frameworks 'react)
            (add-to-list 'js2-imenu-enabled-frameworks 'react)
            ;;'(sp-local-pair 'js2-mode "<" ">")

            ;; Rules
            (setq-default js2-show-parse-errors nil)
            (setq-default js2-strict-trailing-comma-warning nil)
            (setq-default js2-strict-missing-semi-warning nil)
            (setq-default js2-missing-semi-one-line-override t)))

(use-package js2-refactor
  :ensure t
  :config (js2r-add-keybindings-with-prefix "C-c C-m"))

;; ef is extract-function: Extracts the marked expressions out into a new named function.
;; em is extract-method: Extracts the marked expressions out into a new named method in an object literal.
;; ip is introduce-parameter: Changes the marked expression to a parameter in a local function.
;; lp is localize-parameter: Changes a parameter to a local var in a local function.
;; eo is expand-object: Converts a one line object literal to multiline.
;; co is contract-object: Converts a multiline object literal to one line.
;; eu is expand-function: Converts a one line function to multiline (expecting semicolons as statement delimiters).
;; cu is contract-function: Converts a multiline function to one line (expecting semicolons as statement delimiters).
;; ea is expand-array: Converts a one line array to multiline.
;; ca is contract-array: Converts a multiline array to one line.
;; wi is wrap-buffer-in-iife: Wraps the entire buffer in an immediately invoked function expression
;; ig is inject-global-in-iife: Creates a shortcut for a marked global by injecting it in the wrapping immediately invoked function expression
;; ag is add-to-globals-annotation: Creates a /*global */ annotation if it is missing, and adds the var at point to it.
;; ev is extract-var: Takes a marked expression and replaces it with a var.
;; iv is inline-var: Replaces all instances of a variable with its initial value.
;; rv is rename-var: Renames the variable on point and all occurrences in its lexical scope.
;; vt is var-to-this: Changes local var a to be this.a instead.
;; ao is arguments-to-object: Replaces arguments to a function call with an object literal of named arguments.
;; 3i is ternary-to-if: Converts ternary operator to if-statement.
;; sv is split-var-declaration: Splits a var with multiple vars declared, into several var statements.
;; ss is split-string: Splits a string.
;; uw is unwrap: Replaces the parent statement with the selected region.
;; lt is log-this: Adds a console.log statement for what is at point (or region).
;; sl is forward-slurp: Moves the next statement into current function, if-statement, for-loop or while-loop.
;; ba is forward-barf: Moves the last child out of current function, if-statement, for-loop or while-loop.
;; k is kill: Kills to the end of the line, but does not cross semantic boundaries.
;; There are also some minor conveniences bundled:

;; C-S-down and C-S-up moves the current line up or down. If the line is an element in an object or array literal, it makes sure that the commas are still correctly placed.

(use-package json-mode :ensure t)
(use-package json-reformat :ensure t)
(use-package skewer-mode :ensure t)

(use-package jsx-mode
  :ensure t
  :config (progn
            (setq jsx-indent-level 2)))

(provide 'setup-js-mode)
;;; setup-js-mode.el ends here
