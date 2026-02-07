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

(require 'async)

(defvar-local ii/ui-quick-request-overlays nil
  "Current overlays for quick requests.")

(defvar-keymap ii/ui-placeholder-overlay-map
  :doc "Parent map for placeholder overlays.")

(defun ii/ui--truncate-overlay-str (body ov)
  (if (< (length body) 150)
      (progn (overlay-put ov 'ii/ui-expanded nil)
             (overlay-put ov 'ii/ui-full-content body)
             (overlay-put ov 'display (concat body "\n")))
    (overlay-put ov 'ii/ui-full-content body)
    (overlay-put ov 'ii/ui-expanded nil)
    (overlay-put ov 'display (concat (string-truncate-left body 147) "...\n"))))

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

(defun ii/ui-placeholder-animate-elipsis (ov)
  (let ((footer (overlay-get ov 'after-string)))
    (cond ((string-suffix-p "...\n" footer) (overlay-put ov 'after-string (string-trim-right footer "\.*\n")))
          (t (overlay-put ov 'after-string (concat footer "...\n"))))))

(defun ii/ui-put-placeholder ()
  (interactive)
  (ii/ui-make-placeholder-overlay (propertize "HEADER" 'face '(:box (:line-width -1 :color t)))
                                  "BODY" "Waiting"
                                  'error
                                  #'ii/ui-placeholder-animate-elipsis 0.5
                                  (lambda (ov) (message "REPORT"))
                                  (lambda (ov) (message "Finished"))))

(defun ii/ui--format-confirmation-header (ov)
  (concat (propertize "Accept " 'face 'success)
          (propertize "Reject " 'face 'error)
          (propertize "Hide " 'face 'minibuffer-prompt)))

(defun ii/ui--format-confirmation-content (ov content)
  )

(defun ii/ui-make-confirmation-overlay (content)
  (let ((ov (make-overlay (point) (1+ (point)) (current-buffer) t)))
    (overlay-put ov 'before-string (ii/ui--format-confirmation-header ov))
    (overlay-put ov 'display (ii/ui--format-confirmation-content ov content))
    (add-to-list 'ii/ui-pending-confirmation-overlays ov)
    ov))

(defun ii/ui-delete-placeholder-overlay (ov)
  "Delete OV."
  (when (overlayp ov)
    (delq ov ii/ui-quick-request-overlays)
    (delete-overlay ov)))

(provide 'implicit-ui)

;;; implicit-ui.el ends here
