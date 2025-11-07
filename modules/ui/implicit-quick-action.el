;;; -*- lexical-binding: t -*-

(defvar +quick-action-id-index 0)

(defvar +quick-action-overlays nil
  "List of overlays managed by `+windows/quick-action'.")

(defvar +quick-action-id-chars '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\; ?\'))

(defvar +windows-quick-action-alist '((?d . +quick-action-delete)))

(defun +quick-action-add-overlays-to-window (window)
  (setq +quick-action-id-index (1+ +quick-action-id-index))
  (let* ((start (window-start window))
         (wnd-ov (make-overlay start
                               (window-end window)))
         (id-ov (make-overlay start
                              (1+ start)))
         (id-char (elt +quick-action-)))
    (overlay-put wnd-ov 'face 'shadow)
    (overlay-put wnd-ov 'quick-action t)
    (push wnd-ov +quick-action-overlays)
    (overlay-put id-ov 'display (char-to-string
                                 (elt +quick-action-overlays
                                      +quick-action-id-index)))
    (overlay-put id-ov 'quick-action t)
    (overlay-put id-ov 'face 'error)
    (push id-ov +quick-action-overlays)
    ;;(push (cons window ))
    ))

(defun +quick-action-add-overlays ()
  (walk-windows '+quick-action-add-overlays-to-window nil nil))

(defun +quick-action-remove-overlays ()
  (dolist (ov +quick-action-overlays)
    (overlay-put 'evaporate t)))

(defun +quick-action-run-action (char)
  (when-let ((fn (alist-get char +quick-action-alist)))
    (funcall fn)))

(defun +windows/quick-action ()
  (interactive)
  (+quick-action-add-overlays)
  (let ((char (read-char-choice "" ) ))
    (if (not (alist-get char +quick-action-alist))
        (message "Key %c is not associated with a command." char)
      (+quick-action-run-action char)))
  (+quick-action-remove-overlays))


(provide 'implicit-quick-action)
