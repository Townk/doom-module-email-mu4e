;;; email/mu4e/autoload/email.el -*- lexical-binding: t; -*-

(defun +mu4e--account-color-code-mark (face)
  "Return a standar color band to be used aside the message on
the headers view. The bband color is given by FACE."
  (propertize
   " " 'display
   (let ((foreground (or (face-foreground face nil t) "None"))
         (background (or (face-background face nil t) "None")))
     (ignore-errors
       (create-image (format "/* XPM */
                             static char * XFACE[] = {
                                 /* <Values> */
                                 /* <width/columns> <height/rows> <colors> <chars per pixel>*/
                                 \"3 10 2 1\",
                                 /* <Colors> */
                                 \". c %s\",
                                 \"_ c %s\",
                                 /* <Pixels> */
                                 \"...\",
                                 \"...\",
                                 \"...\",
                                 \"...\",
                                 \"...\",
                                 \"...\",
                                 \"...\",
                                 \"...\",
                                 \"...\",
                                 \"...\"
                             };"          ;; ref.: https://en.wikipedia.org/wiki/X_PixMap
                             foreground
                             background)  ;; FILE-OR-DATA
                     'xpm                 ;; TYPE
                     t                    ;; DATA-P
                     :ascent 'center      ;; PROPS
                     )))))


;;;###autoload
(defun set-email-account! (label &rest configuration)
  "Registers an email account for mu4e.

  The LABEL must be the base directory name for this account. For
  instance, if you have your mu4e maildir configured to
  `~/.maildir', and you want to add this account to
  `~/.maildir/AwesomeAccount', then, this account's label should
  be `AwesomeAccount'.

  The CONFIGURATION is a property list with all values necessary
  to configure this account:

    + `:fullname':: Your full name used in this account.

    + `:email':: The main email used in this account.

    + `:aliases'::(optional) A list of strings with all email
        addresses associated with this account. Default to `()'.

    + `:signature':: (optional) A string or a property list where
        keys are the email associated with the desired signature.

    + `:face':: (optional) The face used to extract the account
        color when color-coding is enabled. Default value is
        retrieved from the `+mu4e-account-colors-default' on the
        position equals to the number of configured accounts. If
        there are more accounts than faces defined in
        `+mu4e-account-colors-default', the selected face might
        cycle back to the begining of the array.

    + `:folders:':: (optional) A property list with a mapping for
        all mu4e supported folders. Default value is:
        (`:inbox' \"Inbox\"
         `:drafts' \"Drafts\"
         `:sent' \"Sent\
         `:archive' \"Archive\"
         `:trash' \"Trash\")

    + `:type':: (optional) A symbol indicating what kind of
        account this is. Valid values are `gmail', `outlook',
        `default'. Default to `default'

    + `:default':: (optional) A boolean indicating that this
        account should be the default account.

    + `:vars':: (optional) A list of cons cells
        (VARIABLE . VALUE) passed to the `make-mu4e-context'
        function. You might want to modify:

        + `smtpmail-smtp-*' (required for sending mail from Emacs)"
  (after! mu4e
    (let ((fullname     (plist-get configuration :fullname))
          (email        (plist-get configuration :email)))

      (unless (and label fullname email)
        (mu4e-error "A mu4e account requires at minimum a
                    `label', the user's `fullname', and the
                    user's main `email'."))

      (let* ((type             (or  (plist-get configuration :type) 'default))
             (signature             (plist-get configuration :signature))
             (signature        (and (not (null signature))
                                    (concat "\n\n#+begin_signature\n"
                                            signature
                                            "\n#+end_signature\n")))
             (aliases          (or  (plist-get configuration :aliases) '()))
             (default-p             (plist-get configuration :default))
             (face             (or  (plist-get configuration :face)
                                    (nth (% (length mu4e-contexts)
                                            (length +mu4e-account-colors-default))
                                         +mu4e-account-colors-default)))
             (folders               (plist-get configuration :folders))
             (folder-inbox     (format "/%s/%s" label (or (plist-get folders :inbox) "Inbox")))
             (folder-drafts    (format "/%s/%s" label (or (plist-get folders :drafts) "Drafts")))
             (folder-sent      (format "/%s/%s" label (or (plist-get folders :sent) "Sent")))
             (folder-archive   (format "/%s/%s" label (or (plist-get folders :archive) "Archive")))
             (folder-trash     (format "/%s/%s" label (or (plist-get folders :trash) "Trash")))
             (all-emails       (cons email aliases))
             (signature-extra `((mu4e-compose-signature-auto-include . ,(not (null signature)))
                                (if (and (featurep! :lang org) (featurep! +org))
                                    (org-msg-signature . ,signature)
                                  (mu4e-compose-signature . ,signature))))
             (context
              (make-mu4e-context
               :name label
               :enter-func (lambda () (mu4e-message "Switched to %s" label))
               :leave-func #'mu4e-clear-caches
               :match-func (lambda (msg)
                             (when msg
                               (string-prefix-p (format "/%s" label)
                                                (mu4e-message-field msg :maildir))))
               :vars (append `((user-mail-address      . ,email)
                               (user-full-name         . ,fullname)

                               (mu4e-drafts-folder     . ,folder-drafts)
                               (mu4e-sent-folder       . ,folder-sent)
                               (mu4e-refile-folder     . ,folder-archive)
                               (mu4e-trash-folder      . ,folder-trash)
                               (mu4e-maildir-shortcuts . ((:maildir ,folder-inbox   :key ?i)
                                                          (:maildir ,folder-drafts  :key ?d)
                                                          (:maildir ,folder-sent    :key ?s)
                                                          (:maildir ,folder-archive :key ?a)
                                                          (:maildir ,folder-trash   :key ?t)))
                               (org-msg-signature      . ,signature)
                               (mu4e-compose-signature-auto-include . ,(not (null signature)))

                               (+mu4e-account-mark     . ,(+mu4e--account-color-code-mark face))
                               (+mu4e-account-emails   . ,all-emails)

                               (message-citation-line-format . ,(if (eq type 'outlook)
                                                                    "On %a, %b %d %Y, %N wrote...\n"
                                                                  "On %a, %b %d %Y, %N wrote:\n")))
                             signature-extra
                             (plist-get configuration :vars)))))

        (setq mu4e-contexts
              (cl-loop for context in mu4e-contexts
                       unless (string= (mu4e-context-name context) label)
                       collect context))

        (push context mu4e-contexts)

        (when (> (length mu4e-contexts) 1)
          (setq mu4e-bookmarks
                (cl-loop for bm in mu4e-bookmarks
                         unless (string= (plist-get bm :name) "All inboxes")
                         collect bm))
          (cl-pushnew (make-mu4e-bookmark :name "All inboxes"
                                          :query (mapconcat
                                                  'identity
                                                  (mapcar (lambda (ctx)
                                                            (format "maildir:/%s/Inbox"
                                                                    (mu4e-context-name ctx)))
                                                          mu4e-contexts)
                                                  " OR ")
                                          :key ?a)
                      mu4e-bookmarks))

        (when default-p
          (setq-default mu4e-context-current context))
        context))))
