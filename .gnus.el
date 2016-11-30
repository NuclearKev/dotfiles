;;; gnus.el --- My gnus.el file

;;; Commentary:
;;
;; Custom gnus.el file for my email viewing needs.  As you can see, it's for my
;; super secret Openmailbox account.  Shh!
;;
;; Replace <email-address> with your actual address.  I have it this way so I
;; don't get random emails from people.

;;; Code:

(package-initialize)

(setq user-mail-address "<email-address>")
(setq user-full-name "Kevin Bloom")

(load-library "starttls")
(setq gnus-select-method '(nnimap "openmailbox"
                                  (nnimap-address "imap.openmailbox.org")
                                  (nnimap-server-port 993)))

(setq gnus-secondary-select-methods '((nntp "news.gmane.org")))

(setq-default message-send-mail-function 'smtpmail-send-it
              smtpmail-starttls-credentials '(("smtp.openmailbox.org" 587 nil nil))
              smtpmail-auth-credentials '(("smtp.openmailbox.org" 587
                                           "<email-address>" nil))
              smtpmail-default-smtp-server "smtp.openmailbox.org"
              smtpmail-smtp-server "smtp.openmailbox.org"
              smtpmail-smtp-service 587)

(setq gnus-visible-headers "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^X-Sent:\\|^User-Agent:\\|^X-Mailer:\\|^X-Newsreader:")

(setq gnus-sorted-header-list '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:" "^Followup-To:" "^To:" "^Cc:" "^Date:" "^User-Agent:" "^X-Mailer:" "^X-Newsreader:"))

(setq gnus-user-agent nil)
