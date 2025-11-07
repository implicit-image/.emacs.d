;;; -*- lexical-binding: t -*-
(require 'project)
(require 'vc)
(require 'files)

(defun +mode-line--directory ()
  (abbreviate-file-name (cond ((eq major-mode 'dired-mode) (file-name-parent-directory default-directory))
                              (t default-directory))))

(defun +mode-line--shorten-directory (dir)
  (let* ((split (file-name-split dir))
         (bufname (buffer-name))
         (dirname (concat (car split)
                          "/"
                          (mapconcat
                           (lambda (segment)
                             (seq-take segment 3))
                           (cdr split)
                           "/"))))
    (when (not (string-equal bufname dirname))
      dirname)))

(defun +mode-line--buffer-directory-update (&rest _)
  (setq-local +mode-line--buffer-directory (let ((project (project-current))
                                                 (def (+mode-line--directory)))
                                             (if project
                                                 (concat (project-name project) "/")
                                               (if (length> def 20)
                                                   (+mode-line--shorten-directory def)
                                                 def))))
  (force-mode-line-update))

(defun +mode-line--vc-update (&rest _)
  "Update modeline vc string."
  (when (and (vc-root-dir)
             (buffer-file-name))
    (setq-local +mode-line--vc-text
                (let* ((backend (vc-backend buffer-file-name))
                       (state (vc-state buffer-file-name backend))
                       (icon (cond ((memq state '(edited added)) "+")
                                   ((eq state 'needs-merge) "^")
                                   ((eq state 'needs-update) "v")
                                   ((eq state 'removed) "-")
                                   ((eq state 'conflict) "!")
                                   ((eq state 'unregistered) "=")
                                   (t "?")))
                       (face (pcase icon
                               ("+" 'success)
                               ("^" 'success)
                               ("v" 'warning)
                               ("-" 'error)
                               ("!" 'error)
                               ("=" 'vc-dir-status-ignored)
                               (_ 'vc-dir-status-ignored))))
                  (concat (propertize icon 'face face)
                          " "
                          (if (eq icon "?")
                              "none"
                            (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2)))))))
  (force-mode-line-update))

(defun +mode-line--remote-update (&rest _)
  (setq-local +mode-line--remote-status (when (file-remote-p (buffer-file-name))
                                          (propertize "%@" 'face 'font-lock-escape-face)))
  (force-mode-line-update))

(defun +mode-line--mod-update (&rest _)
  (if (or (buffer-modified-p)
          buffer-read-only)
      (setq-local +mode-line--mod-status (propertize "%*" 'face 'error))
    (setq-local +mode-line--mod-status "")))


(provide 'implicit-modeline)
