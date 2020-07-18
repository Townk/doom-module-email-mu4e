
;;;###autoload
(defun =mu4e ()
  "Start email client."
  (interactive)
  (require 'mu4e)
  (if (featurep! :ui workspaces)
      (progn
        (unless (+workspace-buffer-list)
          (+workspace-delete (+workspace-current-name)))
        (+workspace-switch +mu4e-workspace-name t))
    (setq +mu4e--old-wconf (current-window-configuration))
    (delete-other-windows)
    (switch-to-buffer (doom-fallback-buffer)))
  (mu4e~start 'mu4e~main-view))


;;;###autoload
(defun +mu4e/compose ()
  "Compose a new email."
  (interactive)
  ;; TODO Interactively select email account
  (call-interactively #'mu4e-compose-new))


;;
;; Hooks

;;;###autoload
(defun +mu4e-init-h ()
  "TODO"
  (add-hook 'kill-buffer-hook #'+mu4e-kill-mu4e-h nil t))


(defun +mu4e-kill-mu4e-h ()
  "TODO"
  (cond
   ((and (featurep! :ui workspaces) (+workspace-exists-p +mu4e-workspace-name))
    (+workspace/delete +mu4e-workspace-name))

   (+mu4e--old-wconf
    (set-window-configuration +mu4e--old-wconf)
    (setq +mu4e--old-wconf nil))))
