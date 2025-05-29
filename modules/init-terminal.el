;;; -*- lexical-binding: t -*-

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
  "Open terminal. If NEW-WINDOW is not `nil' open it in new window instead of the current one. Tries to detect current project."
  (require 'projectile)
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
  "Popup an existing terminal or a new one if CREATE-NEW is non-nil."
  (interactive)
  (let ((buffers (mapcar 'buffer-name (+terminal--get-buffers)))
        (action '((display-buffer-below-selected)
                  (window-height . 0.3)
                  (dedicated . t))))
    (pcase (length buffers)
      (1 (pop-to-buffer (-first-item buffers) action))
      (_ (consult--read buffers
                        :prompt "Popup vterm"
                        :default 1
                        :require-match nil
                        :lookup (lambda (buffer &rest args)
                                  (interactive)
                                  (pop-to-buffer buffer action)))))))

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
  (consult--read (if (boundp '+terminal/run-command-history)
                     +terminal/run-command-history
                   '())
                 :prompt "Run in Vterm: "
                 :default 0
                 :require-match nil
                 :history '+terminal/run-command-history
                 :lookup (lambda (command &rest args)
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
(defun +terminal/consult-vterm ()
  (interactive)
  (consult--read (mapcar #'buffer-name (+terminal--get-buffers))
                 :prompt "Vterm: "
                 :annotate 'marginalia-annotate-buffer
                 :default 1
                 :require-match t
                 :lookup (lambda (buffer &rest args)
                           (switch-to-buffer buffer nil nil))))


;; (use-package shell
;;   :straight nil)

(use-package vterm
  :init
  (+set-tab-function! vterm-mode vterm-send-tab)
  :config
  (setq vterm-shell (exec-path-from-shell-getenv "SHELL")))

(use-package eat)

(use-package eee
  :straight (eee :type git
                 :host github
                 :repo "eval-exec/eee.el"
                 :files (:defaults "*.el" "*.sh"))
  :config
  (require 'exec-path-from-shell)
  (setq ee-terminal-command (cond ((or (+os/is-windows-p)
                                       (+os/is-wsl-p))
                                   "wezterm")
                                  (t (exec-path-from-shell-getenv "TERM")))))

;; (bind-keys
;;  :map override-global-map
;;  ("C-x <space> o t c" . +terminal/consult-vterm)
;;  ("C-x <space> o t p" . +terminal/popup)
;;  ("C-x <space> o t !" . +terminal/run-command)
;;  ("C-x <space> o T" . +terminal/open))

(provide 'init-terminal)
