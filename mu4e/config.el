;;; email/mu4e/config.el -*- lexical-binding: t; -*-

;;
;;; Faces

(defface +mu4e-mail-status-mark-face '((t (:foreground "#0885FF")))
  "Used to paint the new or unread mark in front of a message")

(defface +mu4e-message-personal-type-face '((t (:foreground "#3e81de")))
  "Used to paint the message type mark `personal' in front of a message")

(defface +mu4e-message-notification-type-face '((t (:foreground "#00d24a")))
  "Used to paint the message type mark `notification' in front of a message")

(defface +mu4e-message-newsletter-type-face '((t (:foreground "#f5a623")))
  "Used to paint the message type mark `newsletter' in front of a message")

(defface +mu4e-account-blue-face '((t (:foreground "#3A78CC")))
  "A standart blue color for the accounot mark")

(defface +mu4e-account-yellow-face '((t (:foreground "#FFCC02")))
  "A standart yellow color for the accounot mark")

(defface +mu4e-account-brown-face '((t (:foreground "#A67A64")))
  "A standart brown color for the accounot mark")

(defface +mu4e-account-green-face '((t (:foreground "#00cd00")))
  "A standart brown color for the accounot mark")

(defface +mu4e-account-pink-face '((t (:foreground "#eb03af")))
  "A standart brown color for the accounot mark")


;;
;;; Vars

(defvar +mu4e-backend 'mbsync
  "Which backend to use. Can either be offlineimap, mbsync or
  nil (manual).")

(defvar +mu4e-store-maildir "~/.mail"
  "Where mu4e should look for emails. On mu >=1.4, this variable
  is only used to define the attachments location.")

(defvar +mu4e-store-attachments ".attachments"
  "The directory name, inside ~+mu4e-store-maildir~, where
  attachments should be saved.")

(defvar +mu4e-use-all-the-icons nil
  "Enable or disable the use of ~all-the-icons~ icons throughout
  the interface.")

(defvar +mu4e-use-fancy-chars t
  "Allow mu4e to use unicode characters to display marks on the
  UI. When `+mu4e-use-all-the-icons' is non-nil, this variable is
  always evaluated to t.")

(defvar +mu4e-get-mail t
  "Indicate if mu4e should issue a shell comment (defined by the
  ~mu4e-get-mail-command~ variable) to retrieve new email. If
  this variable is non-nil, when you update your emails with
  `mu4e-update-mail-and-index', mu4e will run the shell command
  defined on `mu4e-get-mail-command', otherwise, only the ~mu
  index~ happens.")

(defvar +mu4e-account-color-coding t
  "Use colors to identify an email account. This option is useful
  if you want to have more than one acount setup. This way, the
  message headers will not dedicate a column to display the
  Account, and each line will have a small colored bar at the
  beginning indicating which account the message is associated
  with.")

(defvar +mu4e-account-colors-default '(+mu4e-account-blue-face
                                       +mu4e-account-yellow-face
                                       +mu4e-account-brown-face
                                       +mu4e-account-green-face
                                       +mu4e-account-pink-face)
  "TODO")

(defvar +mu4e-message-typing t
  "TODO")

(setq +mu4e-account-emails '()
      +mu4e-account-mark "")

(when (featurep! :ui workspaces)
  (defvar +mu4e-workspace-name "*Email*"
    "TODO")

  (defvar +mu4e--old-wconf nil
    "TODO"))


(defvar +mu4e-update-interval nil
  "TODO")

;;
;;; Functions


;;
;;; Packages

(use-package! mu4e
  :commands mu4e mu4e-compose-new message-mail
  :init
  (provide 'html2text) ; disable obsolete package

  :config
  (custom-set-faces!
    '(mu4e-header-highlight-face :background "#36383c")
    '(mu4e-unread-face :foreground "#9FB8C7" :bold t)
    '(mu4e-replied-face :inherit mu4e-header-face)
    '(mu4e-draft-face :italic t :foreground "#777777"))

  (defvar +mu4e-message-type-icons
    `(default ,(+mu4e--header-message-type-mark 'default)
       newsletter ,(+mu4e--header-message-type-mark 'newsletter)
       notification ,(+mu4e--header-message-type-mark 'notification))
    "TODO")

  (defvar +mu4e-message-type-func '+mu4e--message-type
    "TODO")

  (pcase +mu4e-backend
    (`mbsync
     (setq mu4e-get-mail-command (if +mu4e-get-mail "mbsync -a" "true")
           mu4e-change-filenames-when-moving t))
    (`offlineimap
     (setq mu4e-get-mail-command (if +mu4e-get-mail "offlineimap -o -q" "true"))))

  (setq mu4e-attachment-dir (let*
                                ((maildir-dir (file-name-as-directory +mu4e-store-maildir))
                                 (maildir-attachments (concat maildir-dir +mu4e-store-attachments)))
                              (cond ((file-exists-p +mu4e-store-attachments) +mu4e-store-attachments)
                                    ((file-exists-p maildir-attachments) maildir-attachments)
                                    (t (concat maildir-dir ".attachments"))))
        mu4e-update-interval +mu4e-update-interval
        mu4e-compose-format-flowed t    ; visual-line-mode + auto-fill upon sending
        mu4e-view-show-addresses t
        mu4e-sent-messages-behavior 'sent
        mu4e-hide-index-messages t
        ;; try to show images
        mu4e-view-show-images t
        mu4e-view-image-max-width 800
        ;; configuration for sending mail
        message-send-mail-function #'smtpmail-send-it
        smtpmail-stream-type 'starttls
        message-kill-buffer-on-exit t   ; close after sending
        ;; start with the first (default) context;
        mu4e-context-policy 'pick-first
        ;; compose with the current context, or ask
        mu4e-compose-context-policy 'ask-if-none
        ;; use helm/ivy
        mu4e-completing-read-function
        (cond ((featurep! :completion ivy) #'ivy-completing-read)
              ((featurep! :completion helm) #'completing-read)
              (t #'ido-completing-read))
        ;; no need to ask
        mu4e-confirm-quit nil
        mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶")
        mu4e-headers-thread-orphan-prefix '("┬>" . "┬▶")
        mu4e-headers-thread-last-child-prefix '("└>" . "╰▶")
        mu4e-headers-thread-child-prefix '("├>" . "├▶")
        mu4e-headers-thread-connection-prefix '("│" . "│ "))

  ;; set mail user agent
  (setq mail-user-agent 'mu4e-user-agent
        message-mail-user-agent 'mu4e-user-agent)


  (if +mu4e-account-color-coding
      (setq mu4e-headers-fields '((:status . 1)
                                  (:symbol-from . 50)
                                  (:aligned-flags . 6)
                                  (:symbol-subject . 82)
                                  (:human-date . 12)
                                  ))
    (setq mu4e-headers-fields '((:account . 12)
                                (:status . 1)
                                (:symbol-from . 44)
                                (:aligned-flags . 6)
                                (:symbol-subject . 55)
                                (:human-date . 12)
                                )))

  (setq mu4e-headers-visible-flags '(draft
                                     flagged
                                     trashed
                                     attach
                                     encrypted
                                     signed))

  (setq mu4e-bookmarks `(,(make-mu4e-bookmark :name "Follow up messages"
                                              :query "flag:flagged"
                                              :key ?f)
                         ,(make-mu4e-bookmark :name "Unread messages"
                                              :query "flag:unread"
                                              :key ?u)
                         ,(make-mu4e-bookmark :name "Today's messages"
                                              :query "date:today..now"
                                              :key ?t)
                         ,(make-mu4e-bookmark :name "Last 7 days"
                                              :query "date:7d..now"
                                              :key ?w)))

  ;; Use fancy icons
  (setq mu4e-use-fancy-chars (or +mu4e-use-all-the-icons +mu4e-use-fancy-chars)
        mu4e-headers-draft-mark (+mu4e-headers-mark 'draft)
        mu4e-headers-flagged-mark (+mu4e-headers-mark 'flagged)
        mu4e-headers-passed-mark (+mu4e-headers-mark 'forwarded)
        mu4e-headers-replied-mark (+mu4e-headers-mark 'replied)
        mu4e-headers-trashed-mark (+mu4e-headers-mark 'trashed)
        mu4e-headers-attach-mark (+mu4e-headers-mark 'attachment)
        mu4e-headers-encrypted-mark (+mu4e-headers-mark 'encrypted)
        mu4e-headers-signed-mark (+mu4e-headers-mark 'signed)
        mu4e-headers-seen-mark (+mu4e-headers-mark 'seen)
        mu4e-headers-new-mark (+mu4e-headers-mark 'new)
        mu4e-headers-unread-mark (+mu4e-headers-mark 'unread))

  (setq mu4e-marks `((refile
                      :char ,(+mu4e-headers-mark 'refile)
                      :prompt "refile"
                      :dyn-target (lambda (target msg) (mu4e-get-refile-folder msg))
                      :action (lambda (docid msg target)
                                (mu4e~proc-move docid (mu4e~mark-check-target target) "-N")))

                     (delete
                      :char ,(+mu4e-headers-mark 'delete)
                      :prompt "Delete"
                      :show-target (lambda (target) "delete")
                      :action (lambda (docid msg target) (mu4e~proc-remove docid)))

                     (flag
                      :char ,(+mu4e-headers-mark 'flag)
                      :prompt "+flag"
                      :show-target (lambda (target) "flag")
                      :action (lambda (docid msg target)
                                (mu4e~proc-move docid nil "+F-u-N")))

                     (move
                      :char ,(+mu4e-headers-mark 'move)
                      :prompt "move"
                      :ask-target mu4e~mark-get-move-target
                      :action (lambda (docid msg target)
                                (mu4e~proc-move docid (mu4e~mark-check-target target) "-N")))

                     (read
                      :char ,(+mu4e-headers-mark 'read)
                      :prompt "!read"
                      :show-target (lambda (target) "read")
                      :action (lambda (docid msg target) (mu4e~proc-move docid nil "+S-u-N")))

                     (trash
                      :char ,(+mu4e-headers-mark 'trash)
                      :prompt "dtrash"
                      :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                      :action (lambda (docid msg target)
                                (mu4e~proc-move docid (mu4e~mark-check-target target) "+T-N")))

                     (unflag
                      :char ,(+mu4e-headers-mark 'unflag)
                      :prompt "-unflag"
                      :show-target (lambda (target) "unflag")
                      :action (lambda (docid msg target) (mu4e~proc-move docid nil "-F-N")))

                     (untrash
                      :char ,(+mu4e-headers-mark 'untrash)
                      :prompt "=untrash"
                      :show-target (lambda (target) "untrash")
                      :action (lambda (docid msg target) (mu4e~proc-move docid nil "-T")))

                     (unread
                      :char ,(+mu4e-headers-mark 'markunread)
                      :prompt "?unread"
                      :show-target (lambda (target) "unread")
                      :action (lambda (docid msg target) (mu4e~proc-move docid nil "-S+u-N")))

                     (unmark
                      :char ,(+mu4e-headers-mark 'unmark)
                      :prompt "unmark"
                      :action (mu4e-error "No action for unmarking"))

                     (action
                      :char ,(+mu4e-headers-mark 'action)
                      :prompt "action"
                      :ask-target  (lambda () (mu4e-read-option "Action: " mu4e-headers-actions))
                      :action  (lambda (docid msg actionfunc)
                                 (save-excursion
                                   (when (mu4e~headers-goto-docid docid)
                                     (mu4e-headers-action actionfunc)))))

                     (something
                      :char ,(+mu4e-headers-mark 'something)
                      :prompt "*something"
                      :action (mu4e-error "No action for deferred mark"))))

  ;; Add a column to display what email account the email belongs to, if you're not using
  ;; `+mu4e-account-color-coding'
  (add-to-list 'mu4e-header-info-custom
               (list :account
                     :name "Account"
                     :shortname "Account"
                     :help "Which account this email belongs to"
                     :function '+mu4e--header-column-account))

  (add-to-list 'mu4e-header-info-custom
               (list :status
                     :name "New message mark"
                     :shortname " "
                     :help
                     (concat "Identify if this message is new, "
                             "unread, or seen, using a symbol, "
                             "an unicode character, or a letter "
                             "depending on your configuration.")
                     :function '+mu4e--header-column-new-status))

  (add-to-list 'mu4e-header-info-custom
               (list :symbol-from
                     :name "From or to"
                     :shortname " From / To"
                     :help
                     (concat "Display who this message is from "
                             "and a mark for its account")
                     :function '+mu4e--header-column-from))

  (add-to-list 'mu4e-header-info-custom
               (list :aligned-flags
                     :name "Flags"
                     :shortname " "
                     :help
                     (concat "Display all flags associated with "
                             "the message.")
                     :function '+mu4e--header-column-flags))

  (add-to-list 'mu4e-header-info-custom
               (list :symbol-subject
                     :name "Subject"
                     :shortname "Subject"
                     :help
                     (concat "Message subject with a symbol "
                             "indicating if the message was "
                             "previously replied or forwared.")
                     :function '+mu4e--header-column-subject))

  (defadvice! +mu4e--refresh-current-view-a (&rest _)
    :after #'mu4e-mark-execute-all (mu4e-headers-rerun-search))

  ;; Wrap text in messages
  (setq-hook! 'mu4e-view-mode-hook truncate-lines nil)
  (add-hook! 'mu4e-headers-mode-hook #'(lambda ()
                                         (setq-local tab-width 2
                                                     evil-normal-state-cursor (list nil))))


  ;; Html mails might be better rendered in a browser
  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser))
  (when (and (featurep! +xwidget) (fboundp 'xwidget-webkit-browse-url))
    (add-to-list 'mu4e-view-actions '("xwidget" . mu4e-action-view-with-xwidget) t))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (when (and (featurep! :lang org) (featurep! +org))
    (org-msg-mode-mu4e)))


(use-package! mu4e-alert
  :defer-incrementally t
  :if (or (featurep! +alert) (and (featurep! :ui modeline) (featurep! +modeline)))
  :config
  (mu4e-alert-set-default-style 'notifier)
  (when (featurep! +alert)
    (mu4e-alert-enable-notifications))
  (when (and (featurep! :ui modeline) (featurep! +modeline))
    (setq doom-modeline-mu4e t)
    (mu4e-alert-enable-mode-line-display)))


(use-package! org-msg
  :if (and (featurep! :lang org) (featurep! +org))
  :commands org-msg-mode-mu4e
  ;; :hook (mu4e-compose-mode . org-msg-edit-mode)
  :init
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
        org-msg-startup "fold hidestars indent inlineimages"
        org-msg-text-plain-alternative t)

  :config
  (map! :localleader
        :map org-msg-edit-mode-map
        :desc "send and exit" "s" #'org-ctrl-c-ctrl-c
        :desc "kill buffer"   "d" #'org-msg-edit-kill-buffer-mu4e
        :desc "save draft"    "S" #'message-dont-send
        :desc "attach"        "a" #'org-msg-attach)

  (org-msg-mode 1)
  (add-hook! 'mu4e-compose-mode 'org-msg-edit-mode))


;;
;;; Hooks

(add-hook 'mu4e-main-mode-hook #'+mu4e-init-h)

(add-to-list 'auto-mode-alist '("\\.\\(?:offlineimap\\|mbsync\\)rc\\'" . conf-mode))



;;
;;; Gmail integration

(when (featurep! +gmail)
  (after! mu4e
    ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
    (setq mu4e-sent-messages-behavior 'delete

          ;; don't need to run cleanup after indexing for gmail
          mu4e-index-cleanup nil

          ;; because gmail uses labels as folders we can use lazy check since
          ;; messages don't really "move"
          mu4e-index-lazy-check t)

    ;; In my workflow, emails won't be moved at all. Only their flags/labels are
    ;; changed. Se we redefine the trash and refile marks not to do any moving.
    ;; However, the real magic happens in `+mu4e|gmail-fix-flags'.
    ;;
    ;; Gmail will handle the rest.
    (defun +mu4e--mark-seen (docid _msg target)
      (mu4e~proc-move docid (mu4e~mark-check-target target) "+S-u-N"))

    (delq! 'delete mu4e-marks #'assq)
    (setf (alist-get 'trash mu4e-marks)
          (list :char '("d" . "▼")
                :prompt "dtrash"
                :dyn-target (lambda (_target msg) (mu4e-get-trash-folder msg))
                :action #'+mu4e--mark-seen)
          ;; Refile will be my "archive" function.
          (alist-get 'refile mu4e-marks)
          (list :char '("r" . "▼")
                :prompt "rrefile"
                :dyn-target (lambda (_target msg) (mu4e-get-refile-folder msg))
                :action #'+mu4e--mark-seen))

    ;; This hook correctly modifies gmail flags on emails when they are marked.
    ;; Without it, refiling (archiving), trashing, and flagging (starring) email
    ;; won't properly result in the corresponding gmail action, since the marks
    ;; are ineffectual otherwise.
    (add-hook! 'mu4e-mark-execute-pre-hook
      (defun +mu4e-gmail-fix-flags-h (mark msg)
        (pcase mark
          (`trash  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
          (`refile (mu4e-action-retag-message msg "-\\Inbox"))
          (`flag   (mu4e-action-retag-message msg "+\\Starred"))
          (`unflag (mu4e-action-retag-message msg "-\\Starred")))))))
