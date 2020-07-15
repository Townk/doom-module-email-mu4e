;; -*- no-byte-compile: t; -*-
;;; email/mu4e/packages.el

(when (or (featurep! +alert) (featurep! +modeline))
  (package! mu4e-alert :pin "91f0657c5b..."))

(when (featurep! +org)
  (package! htmlize)
  (package! org-msg))
