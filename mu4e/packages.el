;; -*- no-byte-compile: t; -*-
;;; email/mu4e/packages.el

(when (or (featurep! +alert) (featurep! +modeline))
  (package! mu4e-alert))

(when (featurep! +org)
  (package! htmlize)
  (package! org-msg))
