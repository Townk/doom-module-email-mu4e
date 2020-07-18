
;;;###autoload
(defun +mu4e-alert--iconised-modeline-formatter (mail-count)
  "Formatter used to get the string to be displayed in the mode-line, using all-the-icons.
MAIL-COUNT is the count of mails for which the string is to displayed"
  (when (not (zerop mail-count))
    (concat " "
            (propertize
             (format "%s %d" (all-the-icons-material "mail_outline") mail-count)
             'help-echo (concat (if (= mail-count 1)
                                    "You have an unread email"
                                  (format "You have %s unread emails" mail-count))
                                "\nClick here to view "
                                (if (= mail-count 1) "it" "them"))
             'mouse-face 'mode-line-highlight
             'keymap '(mode-line keymap
                                 (mouse-1 . mu4e-alert-view-unread-mails)
                                 (mouse-2 . mu4e-alert-view-unread-mails)
                                 (mouse-3 . mu4e-alert-view-unread-mails))))))

;;;###autoload
(defun +mu4e-alert--notification-formatter (mail-group all-mails)
  "Default function to format MAIL-GROUP for notification.
ALL-MAILS are the all the unread emails"
  ;; sound from https://www.freedesktop.org/wiki/Specifications/sound-theme-spec/
  (if (> (length mail-group) 1)
      (let* ((mail-count (length mail-group))
             (total-mails (length all-mails))
             (first-mail (car mail-group))
             (title-prefix (format "You have %d unread emails"
                                   mail-count))
             (field-value (mu4e-alert--get-group first-mail))
             (title-suffix (format (pcase mu4e-alert-group-by
                                     (`:from "from %s:")
                                     (`:to "to %s:")
                                     (`:maildir "in %s:")
                                     (`:priority "with %s priority:")
                                     (`:flags "with %s flags:"))
                                   field-value))
             (title (format "%s %s" title-prefix title-suffix)))
        (list :title title
              :body (s-join "\n"
                            (mapcar (lambda (mail)
                                      (format "%s<b>%s</b> • %s"
                                              (cond
                                               ((plist-get mail :in-reply-to) "⮩ ")
                                               ((string-match-p "\\`Fwd:"
                                                                (plist-get mail :subject)) " ⮯ ")
                                               (t "  "))
                                              (truncate-string-to-width (caar (plist-get mail :from))
                                                                        20 nil nil t)
                                              (truncate-string-to-width
                                               (replace-regexp-in-string "\\`Re: \\|\\`Fwd: " ""
                                                                         (plist-get mail :subject))
                                               40 nil nil t)))
                                    mail-group))))
    (let* ((new-mail (car mail-group))
           (subject (plist-get new-mail :subject))
           (sender (caar (plist-get new-mail :from))))
      (list :title sender :body subject))))


;;;###autoload
(defun +mu4e-alert--terminal-notifier (info)
  "This function is a terminal-notifier custom backend that
configure extra options to alow it to have sounds and a different
icoon on the alert."
  (if alert-notifier-command
      (let ((args
             (list "-title"   (alert-encode-string (plist-get info :title))
                   "-appIcon" (or (plist-get info :icon) alert-notifier-default-icon)
                   "-message" (alert-encode-string (plist-get info :message))
                   "-sound" "default"
                   "-sender" "org.gnu.Emacs")))
        (apply #'call-process alert-notifier-command nil nil nil args))
    (alert-message-notify info)))
