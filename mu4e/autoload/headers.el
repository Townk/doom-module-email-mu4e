;;;###autoload
(defadvice! +mu4e-headers--jump-to-maildir (maildir)
  "Show the messages in maildir (user is prompted to ask what
maildir)."
  :override 'mu4e~headers-jump-to-maildir
  (interactive
   (let ((maildir (mu4e-ask-maildir "Jump to maildir: ")))
     (list maildir)))
  (when maildir
    (setq +mu4e-headers--transient-fields +mu4e-folder-headers-fields)
    (mu4e-mark-handle-when-leaving)
    (mu4e-headers-search (format "maildir:\"%s\"" maildir))))


;;;###autoload
(defun +mu4e-headers--adjust-fields-h (&rest args)
  (setq mu4e-headers-fields (or +mu4e-headers--transient-fields +mu4e-normal-headers-fields)
        +mu4e-headers--transient-fields nil))


(defun +mu4e--get-mark (msg-mark)
  "Given a MSG-MARK return the the character representing such
mark. MSG-MARK is a symbol."
  (let ((cell (cl-case msg-mark
                ('draft     mu4e-headers-draft-mark)
                ('flagged   mu4e-headers-flagged-mark)
                ('new       mu4e-headers-new-mark)
                ('passed    mu4e-headers-passed-mark)
                ('replied   mu4e-headers-replied-mark)
                ('seen      mu4e-headers-seen-mark)
                ('trashed   mu4e-headers-trashed-mark)
                ('attach    mu4e-headers-attach-mark)
                ('encrypted mu4e-headers-encrypted-mark)
                ('signed    mu4e-headers-signed-mark)
                ('unread    mu4e-headers-unread-mark))))
    (if +mu4e-use-fancy-chars
        (cdr cell)
      (car cell))))


(defun +mu4e--message-color-code-mark (msg)
  "TODO"
  (let ((context (mu4e-context-determine msg)))
    (with~mu4e-context-vars context
        +mu4e-account-mark)))


(defun +mu4e--headers-from (msg)
  "TODO"
  (let* ((context (mu4e-context-determine msg))
         (maildir (mu4e-message-field msg :maildir))
         (from-cell (mu4e-message-field msg :from))
         (addr-from (cdr-safe (car-safe from-cell)))
         (from (mu4e~headers-contact-str from-cell))
         (to-cell (mu4e-message-field msg :to))
         (addr-to (cdr-safe (car-safe to-cell)))
         (to (mu4e~headers-contact-str to-cell)))
    (with~mu4e-context-vars context
        (if (string= mu4e-sent-folder maildir)
            (concat "To " (if (member addr-to +mu4e-account-emails) "me" to))
          (if (member addr-from +mu4e-account-emails) "me" from)))))


(defun +mu4e--width-for-func (func)
  "TODO"
  (alist-get (car-safe
              (cl-find-if
               (lambda (item)
                 (eq (plist-get (cdr item) :function)
                     func))
               mu4e-header-info-custom))
             mu4e-headers-fields))


(defun +mu4e--msg-from-me-p (msg)
  "TODO"
  (member (cdr-safe (car-safe (mu4e-message-field msg :from)))
          +mu4e-account-emails))


(defun +mu4e--msg-subject-online-order-keyword (msg)
  "TODO"
  (let ((subject (mu4e-message-field msg :subject)))
    (s-matches? (rx (or (seq "your"
                             space
                             (seq (zero-or-one (seq (one-or-more (not space))
                                                    space))
                                  (or "account"
                                      "payment"
                                      "bill"
                                      "order")))
                        (seq "order " (or "confirmation"
                                          "notice"))
                        "shipment"
                        "delivered"
                        "bill is ready"
                        "purchase"
                        "email verification"
                        "cancelled"
                        "cancellation"
                        "order of"
                        "receipt"
                        (seq "appointment " (or "confirmation" "reminder"))))
                subject)))


;;;###autodef
(defun +mu4e--message-type (msg)
  "TODO"
  (cond ((mu4e-message-field msg :mailing-list)        'newsletter)
        ((+mu4e--msg-subject-online-order-keyword msg) 'notification)
        (t                                             'default)))


;;;###autoload
(defun +mu4e--header-column-subject (msg)
  "TODO"
  (let ((context (mu4e-context-determine msg)))
    (with~mu4e-context-vars context
        (let* ((flags (mu4e-message-field msg :flags))
               (tinfo  (mu4e-message-field msg :thread))
               (replied-symbol (when (memq 'replied flags)
                                 (+mu4e--get-mark 'replied)))
               (forwarded-symbol (when (memq 'passed flags)
                                   (+mu4e--get-mark 'passed)))
               (prefix-p (not (and (s-blank-str-p replied-symbol)
                                   (s-blank-str-p forwarded-symbol))))
               (prefix (concat
                        (unless (s-blank-str-p replied-symbol)
                          (format " %s" replied-symbol))
                        (unless (s-blank-str-p forwarded-symbol)
                          (format " %s" forwarded-symbol))))
               (width (+mu4e--width-for-func '+mu4e--header-column-subject))
               (subject (s-replace-all '(("RE: " . "")
                                         ("Re: " . "")) (mu4e-message-field msg :subject))))
          (concat
           (mu4e~headers-thread-prefix tinfo)
           subject
           prefix)))))


;;;###autoload
(defun +mu4e--header-column-flags (msg)
  "TODO"
  (let ((width (+mu4e--width-for-func '+mu4e--header-column-flags))
        (flags (mu4e~headers-flags-str (mu4e-message-field msg :flags))))
    (s-pad-left (- width 2) " " flags)))


;;;###autoload
(defun +mu4e--header-column-from (msg)
  "TODO"
  (let* ((width (+mu4e--width-for-func '+mu4e--header-column-from)))
    (concat (when +mu4e-message-use-category
              (format "%s " (plist-get +mu4e-message-category-icons
                                       (funcall +mu4e-message-category-func msg))))
            (when +mu4e-account-color-coding
              (format "%s " (+mu4e--message-color-code-mark msg)))
            (+mu4e--headers-from msg))))


;;;###autoload
(defun +mu4e--header-column-recipnum (msg)
  (propertize (format "%2d"
                      (+ (length (mu4e-message-field msg :to))
                         (length (mu4e-message-field msg :cc))))
              'face 'mu4e-footer-face))


;;;###autoload
(defun +mu4e--header-column-new-status (msg)
  "TODO"
  (let* ((flags (mu4e-message-field msg :flags))
         (symbol (cond
                  ((memq 'new flags)    (+mu4e--get-mark 'new))
                  ((memq 'unread flags) (+mu4e--get-mark 'unread))
                  (t                    " "))))
    symbol))


;;;###autoload
(defun +mu4e--header-column-account (msg)
  "TODO"
  (let ((maildir (mu4e-message-field msg :maildir))
        (context (mu4e-context-determine msg)))
    (with~mu4e-context-vars context
        (+mu4e-header--colorise (substring maildir 1 (string-match-p "/" maildir 1))
                                :face +mu4e-account-face))))


;;;###autoload
(defun +mu4e--header-column-folder (msg)
  (let ((maildir (mu4e-message-field msg :maildir))
        (width (+mu4e--width-for-func '+mu4e--header-column-folder)))
    (+mu4e-header--colorise (replace-regexp-in-string "\\`.*/" "" maildir))))


;;;###autoload
(defun +mu4e--header-message-type-mark (type)
  "TODO"
  (pcase type
    ('notification
     (cond
      ((and window-system +mu4e-use-all-the-icons) (all-the-icons-faicon
                                                    "bell-o"
                                                    :height 0.5
                                                    :face '+mu4e-message-notification-type-face
                                                    :v-adjust 0.2))
      (+mu4e-use-fancy-chars "⚠")
      (t "!")))
    ('newsletter
     (cond
      ((and window-system +mu4e-use-all-the-icons) (all-the-icons-faicon
                                                    "rss"
                                                    :height 0.65
                                                    :face '+mu4e-message-newsletter-type-face
                                                    :v-adjust 0.2))
      (+mu4e-use-fancy-chars "☰")
      (t "»")))
    ('default
      (cond
       ((and window-system +mu4e-use-all-the-icons) (all-the-icons-octicon
                                                     "mail"
                                                     :height 0.55
                                                     :face '+mu4e-message-personal-type-face
                                                     :v-adjust 0.2))
       (+mu4e-use-fancy-chars "✉")
       (t " ")))))


(defun +mu4e-header--string-size (str)
  "Return the width in pixels of a string in the current
window's default font. If the font is mono-spaced, this
will also be the width of all other printable characters."
  (let ((window (selected-window))
        (remapping face-remapping-alist))
    (with-temp-buffer
      (make-local-variable 'face-remapping-alist)
      (setq face-remapping-alist remapping)
      (set-window-buffer window (current-buffer))
      (insert str)
      (car (window-text-pixel-size)))))


(cl-defun +mu4e-header--colorise (str &key face)
  (let* ((str-sum (apply #'+ (mapcar (lambda (c) (% c 3)) str)))
         (color (or face (nth (% str-sum (length +mu4e-header--colorised-faces))
                              +mu4e-header--colorised-faces))))
    (put-text-property 0 (length str) 'face color str)
    str))


(defvar +mu4e-header--colorised-faces
  '(all-the-icons-lblue
    all-the-icons-purple
    all-the-icons-blue-alt
    all-the-icons-green
    all-the-icons-maroon
    all-the-icons-yellow
    all-the-icons-orange))


(cl-defun +mu4e-headers--def-mark-icon (name &key set color face height v-adjust)
  "Convert :icon declaration to icon"
  (when +mu4e-use-all-the-icons
    (let* ((icon-set (intern (concat "all-the-icons-" (or set "material"))))
           (v-adjust (or v-adjust 0.02))
           (height (or height 0.8))
           (face (or face (intern (concat "all-the-icons-" color))))
           (icon (if (or color face)
                     (apply icon-set `(,name :face ,face :height ,height :v-adjust ,v-adjust))
                   (apply icon-set `(,name  :height ,height :v-adjust ,v-adjust))))
           (icon-width (+mu4e-header--string-size icon))
           (space-width (+mu4e-header--string-size " "))
           (space-factor (- 2 (/ (float icon-width) space-width))))
      (concat (propertize " " 'display `(space . (:width ,space-factor))) icon))))


;;;###autoload
(defun +mu4e-headers-mark (type)
  "Returns a cons-cell to be used as a headers mark. The TYPE is a symbol representing the
desired mark.

Supported marks are:
 + `draft'
 + `flagged'
 + `forwarded'
 + `replied'
 + `trashed'
 + `attachment'
 + `encrypted'
 + `signed'
 + `seen'
 + `new'
 + `unread'"
  (pcase type
    ('draft      `("D" . ,(or (+mu4e-headers--def-mark-icon "edit" :height 0.7 :v-adjust 0.1 :color "dyellow") "")))
    ('trashed    `("T" . ,(or (+mu4e-headers--def-mark-icon "delete" :height 0.7 :v-adjust 0.1) "")))
    ('attachment `("a" . ,(or (+mu4e-headers--def-mark-icon "attach_file" :height 0.7 :color "dblue") "")))
    ('encrypted  `("x" . ,(or (+mu4e-headers--def-mark-icon "enhanced_encryption" :height 0.5 :v-adjust 0.1 :color "yellow") "")))
    ('signed     `("s" . ,(or (+mu4e-headers--def-mark-icon "verified_user" :height 0.5 :v-adjust 0.1 :color "lgreen") "")))
    ('read       `("!" . ,(or (+mu4e-headers--def-mark-icon "drafts" :height 0.5 :v-adjust 0.1) "◼")))
    ('markunread `("?" . ,(or (+mu4e-headers--def-mark-icon "email" :height 0.5 :v-adjust 0.1) "◻")))
    ('move       `("m" . ,(or (+mu4e-headers--def-mark-icon "local_shipping" :height 0.5 :v-adjust 0.1) "▷")))
    ('something  `("*" . ,(or (+mu4e-headers--def-mark-icon "check" :height 0.5 :v-adjust 0.1) "✱")))
    ('action     `("a" . ,(or (+mu4e-headers--def-mark-icon "settings" :height 0.5 :v-adjust 0.1) "◯")))
    ('flagged    `("F" . ,(or (+mu4e-headers--def-mark-icon "pin" :set "octicon" :height 0.7 :v-adjust 0.1 :color "orange") "")))
    ('forwarded  `("P" . ,(or (+mu4e-headers--def-mark-icon "share" :set "faicon" :height 0.7 :v-adjust 0.1) "")))
    ('replied    `("R" . ,(or (+mu4e-headers--def-mark-icon "reply" :set "faicon" :height 0.7 :v-adjust 0.1) "")))
    ('refile     `("r" . ,(or (+mu4e-headers--def-mark-icon "archive" :set "faicon" :height 0.5 :v-adjust 0.1) "▶")))
    ('trash      `("d" . ,(or (+mu4e-headers--def-mark-icon "recycle" :set "faicon" :height 0.5 :v-adjust 0.1) "▼")))
    ('untrash    `("=" . ,(or (+mu4e-headers--def-mark-icon "inbox" :set "faicon" :height 0.5 :v-adjust 0.1) "▲")))
    ('delete     `("D" . ,(or (+mu4e-headers--def-mark-icon "trash-o" :set "faicon" :height 0.5 :v-adjust 0.1) "x")))
    ('flag       `("+" . ,(or (+mu4e-headers--def-mark-icon "thumb-tack" :set "faicon" :height 0.5 :v-adjust 0.1) "✚")))
    ('unflag     `("-" . ,(or (+mu4e-headers--def-mark-icon "minus-circle" :set "faicon" :height 0.5 :v-adjust 0.1) "➖")))
    ('new        `("N" . ,(or (+mu4e-headers--def-mark-icon "fiber_manual_record" :height 0.5 :face '+mu4e-mail-status-mark-face) "")))
    ('unread     `("U" . ,(or (+mu4e-headers--def-mark-icon "radio_button_unchecked" :height 0.4 :face '+mu4e-mail-status-mark-face) "")))
    ('seen       " ")
    ('unmark     " ")))
