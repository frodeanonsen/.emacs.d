(require 'clojure-mode)
(require 'clj-refactor)

(defadvice clojure-test-run-tests (before save-first activate)
  (save-buffer))

(defadvice nrepl-load-current-buffer (before save-first activate)
  (save-buffer))

(define-key clojure-mode-map (kbd "s-j") 'clj-jump-to-other-file)

(define-key clojure-mode-map (kbd "C-.") 'clj-hippie-expand-no-case-fold)

(defun clj-hippie-expand-no-case-fold ()
  (interactive)
  (let ((old-syntax (char-to-string (char-syntax ?/))))
    (modify-syntax-entry ?/ " ")
    (hippie-expand-no-case-fold)
    (modify-syntax-entry ?/ old-syntax)))

(require 'cider)

(define-key cider-repl-mode-map (kbd "<home>") nil)
(define-key cider-repl-mode-map (kbd "C-,") 'complete-symbol)
(define-key cider-mode-map (kbd "C-,") 'complete-symbol)
(define-key cider-mode-map (kbd "C-c C-q") 'nrepl-close)
(define-key cider-mode-map (kbd "C-c C-Q") 'cider-quit)

;; Indent and highlight more commands
(put-clojure-indent 'match 'defun)

;; Enable error buffer popping also in the REPL:
(setq cider-repl-popup-stacktraces t)

;; Specify history file
(setq cider-history-file "~/.emacs.d/nrepl-history")

;; auto-select the error buffer when it's displayed
(setq cider-auto-select-error-buffer t)

;; Enable eldoc in Clojure buffers
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

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

(define-key clojure-mode-map (kbd "C-´") 'live-cycle-clj-coll)

;; Warn about missing nREPL instead of doing stupid things

(defun nrepl-warn-when-not-connected ()
  (interactive)
  (message "Oops! You're not connected to an nREPL server. Please run M-x cider or M-x cider-jack-in to connect."))

(define-key clojure-mode-map (kbd "C-M-x")   'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-x C-e") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-e") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-l") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-r") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-z") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-k") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-n") 'nrepl-warn-when-not-connected)

;; ------------
;; Refactor keybindings
;; - `rf`: rename file, update ns-declaration, and then query-replace new ns in project.
;; - `ar`: add :require to namespace declaration, then jump back
;; - `au`: add :use to namespace declaration, then jump back
;; - `ai`: add :import to namespace declaration, then jump back
;; - `th`: thread another expression
;; - `uw`: unwind a threaded expression

(defun setup-clj-refactor-mode ()
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(defun frode-clojure-mode-hooks ()
  (linum-mode 1)
  (highlight-symbol-mode)
  (fci-mode)
  (auto-complete-mode)
  (setup-clj-refactor-mode))

(defun frode-clojurescript-mode-hooks ()
  (linum-mode 1)
  (highlight-symbol-mode)
  (fci-mode)
  (setup-clj-refactor-mode))

(add-hook 'clojure-mode-hook 'frode-clojure-mode-hooks)
(add-hook 'clojurescript-mode-hook 'frode-clojurescript-mode-hooks)

;; TODO: Loot more stuff from:
;;  - https://github.com/overtone/emacs-live/blob/master/packs/dev/clojure-pack/config/paredit-conf.el


;; Org-mode Babel for Clojure
(require 'cider)
(setq org-babel-clojure-backend 'cider)
(require 'ob-clojure)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (clojure . t)
   (emacs-lisp . t)))

;; Eval w/o confirming
(setq org-confirm-babel-evaluate nil)

;; Clojure-specific
;;
;; org-babel Clojure support is built to work with Swank/SLIME, which is no longer
;; being developed, having been replaced by nREPL. This code switches out the former
;; in favor of the latter. Taken from https://github.com/lambdatronic/org-babel-example

;; Patch ob-clojure to work with nrepl
(declare-function nrepl-send-string-sync "ext:cider" (code &optional ns))

(defun org-babel-execute:clojure (body)
  (cider-read-and-eval body))

(defun org-babel-execute:clojure (body params)
  "Execute a block of Clojure code with Babel."
  (require 'cider)
  (with-temp-buffer
    (insert (org-babel-expand-body:clojure body params))
    ((lambda (result)
       (let ((result-params (cdr (assoc :result-params params))))
         (if (or (member "scalar" result-params)
                 (member "verbatim" result-params))
             result
           (condition-case nil (org-babel-script-escape result)
             (error result)))))
     (plist-get (nrepl-sync-request:eval
                 (buffer-substring-no-properties (point-min) (point-max))
                 (cdr (assoc :package params)))
                :value)
     (cider-read-and-eval (org-babel-expand-body:clojure body params)))))


(provide 'setup-clojure-mode)
