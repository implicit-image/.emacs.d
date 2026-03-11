;;; implicit-ui.el --- ui enhancements -*- lexical-binding: t -*-

;; Author: Błażej Niewiadomski
;; Maintainer: Błażej Niewiadomski
;; Version: version
;; Package-Requires: ()
;; Homepage: homepage
;; Keywords: keywords


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


(defvar-local ii/ui-quick-request-overlays nil
  "Current overlays for quick requests.")

(defvar ii/ui-pending-confirmation-overlays nil)

(defvar-keymap ii/ui-placeholder-overlay-map
  :doc "Parent map for placeholder overlays.")

(defvar-keymap ii/ui-confirmation-overlay-map
  :doc ""
  "<return>" #'ii/ui-overlay-confirm
  "<tab>" #'ii/ui-overlay-toggle-show
  "C-c C-k" #'ii/ui-overlay-reject)

(defun ii/ui--truncate-overlay-str (body ov)
  (if (< (length body) 150)
      (progn (overlay-put ov 'ii/ui-expanded nil)
             (overlay-put ov 'ii/ui-full-content body)
             (overlay-put ov 'display (concat body "\n")))
    (overlay-put ov 'ii/ui-full-content body)
    (overlay-put ov 'ii/ui-expanded nil)
    (overlay-put ov 'display (concat (string-truncate-left body 147) "...\n"))))


;; Overlay functions wrappers

;; Animations
(defun ii/ui-placeholder-animate-elipsis (ov)
  (let ((footer (overlay-get ov 'after-string)))
    (cond ((string-suffix-p "...\n" footer) (overlay-put ov 'after-string (string-trim-right footer "\.*\n")))
          (t (overlay-put ov 'after-string (concat footer "...\n"))))))

(defmacro ii/ui--create-animation! (state &rest frames)
  (if (length> frames 1)
      `(pcase ,state
         ,@(seq-reduce (lambda (prev cur))
                       (cons (list prev cur)))
         (_ ,(car frames)))
    (car frames)))

;; (defun ii/ui-animate-placeholder (ov bar-char)
;;   (let ((val (pcase (string-trim (overlay-get ov 'after-string))
;;                ((make-string) "||")
;;                ("||" "|||")
;;                ("|||" "||||")
;;                (_ "|"))))
;;     (overlay-put ov 'after-string (concat val "\n"))))

(defun ii/ui-make-placeholder-overlay (header body footer face &optional anim-fn anim-rate report-fn callback local-map)
  (let ((ov (make-overlay (point) (1+ (point)) (current-buffer) t)))
    (overlay-put ov 'before-string (concat header "\n"))
    (ii/ui--truncate-overlay-str body ov)
    (overlay-put ov 'after-string (concat footer "\n"))
    (overlay-put ov 'face face)
    (when report-fn
      (overlay-put ov 'ii/ui-report-function report-fn))
    (when callback
      (overlay-put ov 'ii/ui-placeholder-callback callback))
    (when (and anim-fn anim-rate)
      (overlay-put ov 'ii/ui-animation-timer (run-with-timer 0.5 anim-rate (lambda ()
                                                                             (funcall anim-fn ov)))))
    (add-to-list 'ii/ui-quick-request-overlays ov)
    ov))

(defun ii/ui--fail (ov)
  (overlay-put )
  (pulse-momentary-highlight-overlay ov 'error))

(defun ii/ui--format-confirmation-content (ov)
  (let ((map (overlay-get ov 'keymap)))
    (concat
     (propertize "Accept" 'face 'success) " | "
     (when (overlay-get ov 'ii/ui-retry-fn)
       (concat (propertize "Retry" 'face 'consult-file) " | "))
     (propertize "Reject" 'face 'error) " | "
     (propertize (if (overlay-get ov 'ii/ui-hidden) "Show" "Hide") 'face 'warning)
     "\n"
     (when (null (overlay-get ov 'ii/ui-hidden))
       (concat (overlay-get ov 'ii/ui-content) "\n")))))

(defun ii/ui-make-confirmation-overlay (content confirm-fn &optional retry-fn)
  (let ((ov (make-overlay (point) (1+ (point)) (current-buffer) t)))
    ;; (overlay-put ov 'before-string (ii/ui--format-confirmation-header ov))
    (overlay-put ov 'ii/ui-content content)
    (overlay-put ov 'ii/ui-hidden nil)
    (overlay-put ov 'ii/ui-confirm-fn confirm-fn)
    (overlay-put ov 'keymap ii/ui-confirmation-overlay-map)
    (when retry-fn
      (overlay-put ov 'ii/ui-retry-fn retry-fn))
    (overlay-put ov 'display (ii/ui--format-confirmation-content ov))
    (add-to-list 'ii/ui-pending-confirmation-overlays ov)
    ov))

;;;; Safe delete functions
(defun ii/ui-delete-placeholder-overlay (ov)
  "Delete OV."
  (when (overlayp ov)
    (when (timerp (overlay-get ov 'ii/ui-animation-timer))
      (cancel-timer (overlay-get ov 'ii/ui-animation-timer)))
    (delq ov ii/ui-quick-request-overlays)
    (delete-overlay ov)))

(defun ii/ui-delete-confirmation-overlay (ov)
  (when (overlayp ov)
    (delq ov ii/ui-pending-confirmation-overlays)
    (delete-overlay ov)))

;;;; interaction
(defun ii/ui-overlay-confirm (ov)
  (interactive (list (seq-find (lambda (elt)
                                 (and (overlayp elt)
                                      (overlay-get elt 'ii/ui-confirm-fn)))
                               (overlays-at (point)))))
  (when ov
    (funcall (overlay-get ov 'ii/ui-confirm-fn) ov)
    (delq ov ii/ui-pending-confirmation-overlays)))

(defun ii/ui-overlay-reject (ov)
  (interactive (list (seq-find (lambda (elt)
                                 (and (overlayp elt)
                                      (overlay-get elt 'ii/ui-confirm-fn)))
                               (overlays-at (point)))))
  (when ov
    (message "Rejecting!")
    (ii/ui-delete-confirmation-overlay ov)))

(defun ii/ui-overlay-toggle-show (ov)
  (interactive (list (seq-find (lambda (elt)
                                 (and (overlayp elt)
                                      (overlay-get elt 'ii/ui-confirm-fn)))
                               (overlays-at (point)))))
  (when ov
    (overlay-put ov 'ii/ui-hidden (not (overlay-get ov 'ii/ui-hidden)))
    (overlay-put ov 'display (ii/ui--format-confirmation-content ov))))

(defun ii/ui-overlay-retry (ov)
  (interactive (list (seq-find (lambda (elt)
                                 (and (overlayp elt)
                                      (overlay-get elt 'ii/ui-retry-fn)))
                               (overlays-at (point)))))
  (when ov
    (funcall (overlay-get ov 'ii/ui-retry-fn))))

(defun ii/ui-expand-placeholder-overlay (ov)
  (interactive (list (seq-find (lambda (elt)
                                 (and (overlayp elt)
                                      (overlay-get elt 'ii/ui-full-content)))
                               (overlays-at (point)))))
  (when-let* ((full (overlay-get ov 'ii/ui-full-content)))
    (if (not (overlay-get ov 'ii/ui-expanded))
        (progn (overlay-put ov 'ii/ui-expanded t)
               (overlay-put ov 'display (overlay-get ov 'ii/ui-full-content)))
      (let ((truncated (string-truncate-left full 147)))
        (overlay-put ov 'ii/ui-expanded nil)
        (overlay-put ov 'display truncated)))))

(defun ii/ui-report-placeholder-status (ov)
  (interactive (list (seq-find (lambda (elt)
                                 (and (overlayp elt)
                                      (overlay-get elt 'ii/ui-report-function)))
                               (overlays-at (point)))))
  (when ov
    (funcall (overlay-get ov 'ii/ui-report-function) ov)))

(provide 'implicit-ui)

;;; implicit-ui.el ends here
