(require 'cl-lib)
(defun endless/indent-defun ()
  "Indent current defun."
  (interactive)
  (unless (region-active-p)
    (let ((l (save-excursion (beginning-of-defun 1) (point)))
          (r (save-excursion (end-of-defun 1) (point))))
      (cl-letf (((symbol-function 'message) #'ignore))
        (indent-region l r)))))

(defun endless/activate-aggressive-indent ()
  "Locally add `endless/indent-defun' to `post-command-hook'."
  (add-hook 'post-command-hook
            #'endless/indent-defun nil 'local))

(add-hook 'emacs-lisp-mode-hook
          #'endless/activate-aggressive-indent)

(add-hook 'clojure-mode-hook
          #'endless/activate-aggressive-indent)

(add-hook 'clojurescript-mode-hook
          #'endless/activate-aggressive-indent)
