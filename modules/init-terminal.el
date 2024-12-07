(require 'projectile)
(require 'ivy)

(defun +terminal--get-buffers (&optional preds)
  (match-buffers `(or . ,(append '((major-mode . vterm-mode))
			      preds))))

(defun +terminal--get-name ()
  "Get a name for local vterm buffer"
  (string-join `(,vterm-buffer-name ,default-directory)))


(defun +terminal--get-dwim-command (&optional new-window)
  "Open terminal. If NEW-WINDOW is not `nil' open it in new window instead of the current one. Tries to detect current project"
  (let* ((command-suffix (if (and (boundp 'new-window) new-window)
			     "other-window"
			   ""))
	 (command-prefix (if (projectile-project-root)
			     "projectile-run-vterm"
			   "vterm")))
    (intern (string-join `(,command-prefix
			   ,(when (not (string= command-suffix ""))
			      "-")
			   ,command-suffix)))))

;;;###autoload
(defun +terminal/open (&optional other-window)
  "Switches to an existing vterm buffer or creates a new one."
  (interactive)
  (funcall-interactively (+terminal--get-dwim-command other-window)))

;;;###autoload
(defun +terminal/new (&optional other-window)
  "Open new terminal."
  (interactive)
  (funcall-interactively (+terminal--get-dwim-command other-window) t))

;;;###autoload
(defun +terminal/run-command (&optional )
  "Run COMMAND in vterm terminal."
  (interactive)
  (ivy-read "run in vterm: " (if (boundp '+terminal/run-command-history)
				 +terminal/run-command-history
			       '())
	    :preselect 0
	    :require-match nil
	    :history '+terminal/run-command-history
	    :caller '+terminal/run-command
	    :action (lambda (command)
		      (interactive)
		      (let ((buffer (funcall (+terminal--get-dwim-command t))))
			(with-current-buffer buffer
			  (interactive)
			  (when (not (eq evil-state 'insert))
			    (evil-insert-state))
			  (read-only-mode -1)
			  (vterm-send-string command)
			  (vterm-send-return))))))

;;;###autoload
(defun +terminal/counsel-vterm ()
  (interactive)
  (ivy-read "Vterm:" (mapcar #'buffer-name (+terminal--get-buffers))
	    :preselect 0
	    :require-match t
	    :history '+terminal/counsel-vterm-history
	    :action (lambda (buffer)
		      (switch-to-buffer buffer nil nil))
	    :caller '+terminal/counsel-vterm))

(use-package vterm
  :init
  (evil-set-initial-state 'vterm-mode 'insert)
  :hook (vterm-mode . (lambda () (display-line-numbers-mode -1))))

(use-package eat
  :init
  (evil-set-initial-state 'eat-mode 'insert)
  :hook
  (eat-mode . (lambda () (display-line-numbers-mode -1))))

(provide 'init-terminal)
