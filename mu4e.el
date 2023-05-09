;;; mu4e.el -*- lexical-binding: t; -*-

(after! mu4e
  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail)
  ;; set a more visible mu4e view (with dark-mode enabled)
  (setq shr-color-visible-luminance-min 01)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  (setq mu4e-headers-unread-mark    '("u" . "📩 "))
  (setq mu4e-headers-draft-mark     '("D" . "🚧 "))
  (setq mu4e-headers-flagged-mark   '("F" . "🚩 "))
  (setq mu4e-headers-new-mark       '("N" . "✨ "))
  (setq mu4e-headers-passed-mark    '("P" . "↪ "))
  (setq mu4e-headers-replied-mark   '("R" . "↩ "))
  (setq mu4e-headers-seen-mark      '("S" . " "))
  (setq mu4e-headers-trashed-mark   '("T" . "🗑️"))
  (setq mu4e-headers-attach-mark    '("a" . "📎 "))
  (setq mu4e-headers-encrypted-mark '("x" . "🔑 "))
  (setq mu4e-headers-signed-mark    '("s" . "🖊 "))
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

  )
