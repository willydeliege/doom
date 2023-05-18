;;; mu4e.el -*- lexical-binding: t; -*-
(after! mu4e
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(setq mu4e-get-mail-command "mbsync -a")
(setq sendmail-program (executable-find "msmtp")
      send-mail-function #'smtpmail-send-it
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-send-mail-function #'message-send-mail-with-sendmail)
;; set a more visible mu4e view (with dark-mode enabled)
(setq shr-color-visible-luminance-min 60)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
(setq mu4e-use-fancy-chars t)
(setq
 mu4e-headers-draft-mark     '("D" . "💈")
 mu4e-headers-flagged-mark   '("F" . "📍")
 mu4e-headers-new-mark       '("N" . "🔥")
 mu4e-headers-passed-mark    '("P" . "❯")
 mu4e-headers-replied-mark   '("R" . "❮")
 mu4e-headers-seen-mark      '("S" . "☑")
 mu4e-headers-trashed-mark   '("T" . "💀")
 mu4e-headers-attach-mark    '("a" . "📎")
 mu4e-headers-encrypted-mark '("x" . "🔒")
 mu4e-headers-signed-mark    '("s" . "🔑")
 mu4e-headers-unread-mark    '("u" . "⎕")
 mu4e-headers-list-mark      '("l" . "🔈")
 mu4e-headers-personal-mark  '("p" . "👨")
 mu4e-headers-calendar-mark  '("c" . "📅"))
(setq mu4e-update-interval 60)
(setq mu4e-maildir-shortcuts
      '( (:maildir "/INBOX" :key ?i)
         (:maildir "/[Gmail]/Sent Mail"  :key ?S)
         (:maildir "/[Gmail]/Trash" :key ?t)
         (:maildir "/[Gmail]/Starred" :key ?s)
         (:maildir "/[Gmail]/All Mail"   :key ?a)))

(add-to-list 'org-capture-templates
             '("M" "Email Workflow"))
(add-to-list 'org-capture-templates
             '("Mf" "Follow Up" entry (file+olp "~/org/Inbox.org" "Follow Up")
               "* TODO Follow up with %:fromname on %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i" :immediate-finish t))
(add-to-list 'org-capture-templates
             '("Mr" "Read Later" entry (file+olp "~/org/Inbox.org" "Read Later")
               "* TODO Read %:subject\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n\n%i" :immediate-finish t))

(defun my/capture-mail-follow-up (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "Mf"))

(defun my/capture-mail-read-later (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "Mr"))

;; Add custom actions for our capture templates
(add-to-list 'mu4e-headers-actions
             '("follow up" . my/capture-mail-follow-up) t)
(add-to-list 'mu4e-view-actions
             '("follow up" . my/capture-mail-follow-up) t)
(add-to-list 'mu4e-headers-actions
             '("read later" . my/capture-mail-read-later) t)
(add-to-list 'mu4e-view-actions
             '("read later" . my/capture-mail-read-later) t)
(setq org-msg-signature "\n\nRegards,\n\n\n--\n\n*Frédéric Willem*\n\n/One Emacs to rule them all/\n")
;; (autoload 'bbdb-insinuate-mu4e "bbdb-mu4e")
;; (bbdb-initialize 'message 'mu4e)

;; (setq bbdb-mail-user-agent 'mu4e-user-agent)
;; (setq mu4e-view-rendered-hook 'bbdb-mua-auto-update)
;; (setq mu4e-compose-complete-addresses nil)
;; (setq bbdb-mua-pop-up t)
;; (setq bbdb-mua-pop-up-window-size 5)
(setq mu4e-org-contacts-file "/home/willefi/org/contacts.org")
(add-to-list 'mu4e-headers-actions
             '("org-contact-add" . mu4e-action-add-org-contact) t)
(add-to-list 'mu4e-view-actions
             '("org-contact-add" . mu4e-action-add-org-contact) t)
(define-key mu4e-view-mode-map "S-;" 'bbdb-mua-display-sender)
(define-key mu4e-view-mode-map ";" 'bbdb-mua-edit-field-sender))
