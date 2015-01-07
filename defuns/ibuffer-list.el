;; Define ibuffer filter groups for each known project
(defun my/define-projectile-filter-groups ()
  (when (boundp 'projectile-known-projects)
    (setq my/project-filter-groups
          (mapcar
           (lambda (it)
             (let ((name (file-name-nondirectory (directory-file-name it))))
               `(,name (filename . ,(expand-file-name it)))))
           projectile-known-projects))))

;; Set up default ibuffer filter groups
(setq ibuffer-saved-filter-groups
      (list
       (cons "default"
             (append
              (my/define-projectile-filter-groups)
              ;; ... whatever other groups you want, e.g.
              '(("dired" (mode . dired-mode))
                ("erc" (mode . erc-mode)))
              ))))

;; Enable default groups by default
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; You probably don't want to see empty project groups
(setq ibuffer-show-empty-filter-groups nil)

