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

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))
