(setq mu4e-maildir "~/Mail")

(setq mu4e-drafts-folder "/Anonsen/[Gmail].Drafts")
(setq mu4e-sent-folder   "/Anonsen/[Gmail].Sent Mail")
(setq mu4e-refile-folder "/Anonsen/Archive")
(setq user-mail-address "frode@anonsen.org")

(defvar my-mu4e-account-alist
  '(("Anonsen"
     (mu4e-sent-folder "/Anonsen/[Gmail].Sent Mail")
     (mu4e-drafts-folder "/Anonsen/[Gmail].Drafts")
     (mu4e-refile-folder "/Anonsen/Archive")
     (user-mail-address "frode@anonsen.org"))
    ("Knowit"
     (mu4e-sent-folder "/Knowit/[Gmail].Sent Mail")
     (mu4e-drafts-folder "/Knowit/[Gmail].Drafts")
     (mu4e-refile-folder "/Knowit/Archive")
     (user-mail-address "frode.anonsen@knowit.no"))))

;; Trash
(setq mu4e-trash-folder
      (lambda (msg)
        (cond
         ((or (mu4e-message-contact-field-matches msg :to "@knowit")
              (mu4e-message-contact-field-matches msg :cc "@knowit"))
          "/Knowit/[Gmail].Trash")
         ;; important to have a catch-all at the end!
         (t  "/Anonsen/[Gmail].Trash"))))

;; Shortcuts
(setq mu4e-maildir-shortcuts
      '(("/Anonsen/INBOX" . ?a)
        ("/Knowit/INBOX" . ?k)
	("/Anonsen/D5L" . ?d)
	("/readme" . ?r)))

;; sending mail
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/local/bin/msmtp"
      user-full-name "Frode Anonsen")

;; Borrowed from http://ionrock.org/emacs-email-and-mu.html
;; Choose account label to feed msmtp -a option based on From header
;; in Message buffer; This function must be added to
;; message-send-mail-hook for on-the-fly change of From address before
;; sending message since message-send-mail-hook is processed right
;; before sending message.
(defun choose-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
	(let*
	    ((from (or (save-restriction
			 (message-narrow-to-headers)
			 (message-fetch-field "from")) ""))
	     (cc (or (save-restriction
		       (message-narrow-to-headers)
		       (message-fetch-field "cc")) ""))
	     (account
	      (cond
	       ((or (string-match "@anonsen.org" from)
		    (string-match "@anonsen.org" cc)) "anonsen")
	       ((or (string-match "@knowit.no" from)
		    (string-match "@knowit.no" cc)) "knowit")
	       (t "gmail"))))
	  (setq message-sendmail-extra-arguments (list '"-a" account))))))
(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'choose-msmtp-account)

;; Dynamic sender addresses
(add-hook 'mu4e-compose-pre-hook
	  (defun my-set-from-address ()
	    "Set the From address based on the To address of the original."
	    (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
	      (if msg
		  (setq user-mail-address
			(cond
			 ((mu4e-message-contact-field-matches msg :to "@knowit")
			  "frode.anonsen@knowit.no")
			 ((mu4e-message-contact-field-matches msg :to "@anonsen.org")
			  "frode@anonsen.org")
			 (t "frode@anonsen.org")))))))

(setq mu4e-user-mail-address-list
      (list "frode@anonsen.org" "frode.aanonsen@knowit.no"))


;; Dynamic select email accounts when composing new message
(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

;; Custom formats
(setq mu4e-headers-date-format "%Y%m%d %H:%M")

;; Fetch new mail every 5
(setq mu4e-update-interval 300)


;; Maildirs ext
(require 'mu4e-maildirs-extension)
(setq mu4e-maildirs-extension-custom-list
      '( "/Anonsen/INBOX" "/Knowit/INBOX" ))
(mu4e-maildirs-extension)

(provide 'setup-email)
