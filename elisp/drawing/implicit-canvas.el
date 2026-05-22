;;; implicit-canvas.el --- Svg drawing utilities -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; commentary

;;; Code:

;; -*- lexical-binding: t; -*-

(defvar-local ii/canvas-surface nil)

(defvar ii/canvas-default-width 500)

(defvar ii/canvas-default-height 500)

(defvar ii/canvas-input-type 'drawing)

(defvar-local ii/canvas-dragging nil)

(defun ii/canvas--create-surface (start-pos)
  (save-mark-and-excursion
    (goto-char start-pos)
    (let ((inhibit-read-only t))
      (setq-local ii/canvas-surface (svg-create ii/canvas-default-width
                                                ii/canvas-default-height
                                                :stroke "orange"
                                                :stroke-width 5))
      (svg-insert-image ii/canvas-surface))))

(defun ii/canvas-image-xy (posn)
  "If POSN is on an image, return a position '(x . y)' relative
to the image."
  (let ((obj (posn-object posn)))
    (when (eq 'image (car obj))
      (let* ((scale (plist-get (cdr obj) :scale))
             (scale (if (numberp scale) scale 1))
             (posn-xy (posn-object-x-y posn))
             (x (/ (car posn-xy) scale))
             (y (/ (cdr posn-xy) scale)))
        (cons x y)))))


(defun ii/canvas--handle-mouse (event)
  (interactive "e")
  (let* ((inhibit-read-only t)
         (start (ii/canvas-image-xy (event-start event)))
         (end (ii/canvas-image-xy (event-end event))))
    (setq ii/canvas-dragging (not ii/canvas-dragging))
    (svg-line ii/canvas-surface (car start) (cdr start) (car end) (cdr end))))

(defun ii/canvas--handle-mouse-movement (event)
  (interactive "e")
  (message "handling mouse")
  (let ((inhibit-read-only t))
    (when ii/canvas-dragging
      (message "Move event: %S" event))))

(defun ii/canvas--redraw ()
  (goto-char (point-min))
  (erase-buffer)
  (svg- ))

(defvar-keymap ii/canvas-mode-map
  :keymap nil)


(defvar ii/canvas-mouse-positions nil)

(defun ii/canvas--handle-mouse-down (event)
  (interactive "e")
  (message "Event: %S" event)
  (let* (()))
  (track-mouse
    (while-let ((ev (read-event))
                (mouse (and (or (mouse-movement-p ev)
                                (eq (car ev) 'down-mouse-1))
                            (not (eq (car ev) 'up-mouse-1)))))
      (let* ((inhibit-read-only t)
             (start (ii/canvas-image-xy (event-start ev)))
             (end (ii/canvas-image-xy (event-end ev))))
        (push (cons start end) ii/canvas-mouse-positions))))
  (message "finished")
  (let ((inhibit-read-only t)
        (pos (car ii/canvas-mouse-positions)))
    (svg-circle ii/canvas-surface (car pos) (cdr pos) 10))
  ;; (setq ii/canvas-mouse-positions nil)
  )

(define-key ii/canvas-mode-map [drag-mouse-1] #'ii/canvas--handle-mouse)
(define-key ii/canvas-mode-map [down-mouse-1] #'ii/canvas--handle-mouse-down)



(defun ii/canvas--post-command-hook ()
  ())

(define-derived-mode ii/canvas-mode special-mode
  "Canvas"
  "Mode for drawing."
  (progn
    (let ((inhibit-read-only t))
      (setq-local buffer-file-name nil
                  mouse-fine-grained-tracking t)
      (erase-buffer)
      (ii/canvas--create-surface (point-min))
      (use-local-map ii/canvas-mode-map)
      (add-hook 'post-command-hook #'ii/canvas--post-command-hook nil t)
      (dolist (ov (overlays-in (point-min) (point-max)))
        (overlay-put ov 'keymap ii/canvas-mode-map)))))

(provide 'implicit-canvas)
;;; implicit-canvas.el ends here
