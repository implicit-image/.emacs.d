(require 'projectile)

(defun +terminal--get-buffers (&rest preds)
  (match-buffers `(or . ,(append '((major-mode . vterm-mode))
				 preds))))

(defun +terminal--get-annotated-buffers (&rest preds)
  (mapcar (lambda (buffer)
	    (append '(,buffer) (marginalia-annotate-buffer buffer)))
	  (+terminal--get-buffers)))

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
(defun +terminal/popup (&optional create-new)
  "Popup an exiisting terminal or a new one."
  (interactive)
  (ivy-read "Popup vterm" (+terminal--get-buffers)
	    :preselect 1
	    :require-match nil
	    :action (lambda (buffer)
		      (popwin:popup-buffer buffer))
	    :caller '+terminal/popup))

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

(use-package eee
  :straight (eee :type git
		 :host github
		 :repo "eval-exec/eee.el"
                 :files (:defaults "*.el" "*.sh"))
  :init
  (setq ee-terminal-command (exec-path-from-shell-getenv "TERM")))

(+leader-keys
  "o t" '(:ignore t :which-key "Terminal")
  "o t c" '("Switch to other" . +terminal/counsel-vterm)
  "o t p" '("Popup terminal" . +terminal/popup)
  "o t r" '("Run command" . +terminal/run-command)
  "o T" '("Popup terminal" . +terminal/open))

(provide 'init-terminal)
