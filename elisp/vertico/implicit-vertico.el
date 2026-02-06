;;; implicit-vertico.el --- Vertico utils -*- lexical-binding: t -*-

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

(require 'vertico)

(defvar ii/vertico--window-config nil
  "Saved window configuration before opening vertico in `vertico-buffer-mode'.")

(defvar ii/vertico--buffer-width-alist '((0.5 (consult-ripgrep consult-grep affe-grep consult-fd consult-find project-find-file))
                                         (0.35 (consult-imenu consult-org-heading consult-outline)))
  "TODO")

(defun ii/vertico--restore-window-config ()
  (when (window-configuration-p ii/vertico--window-config)
    (set-window-configuration ii/vertico--window-config)
    (setq ii/vertico--window-config nil))
  (remove-hook 'minibuffer-exit-hook 'ii/vertico--restore-window-config))

(defun ii/vertico--buffer-get-display-alist-props (wider command)
  (if (null wider)
      '(:width 1.0 :height 0.3 :side bottom)
    (let* ((width (cond ((memq command (car (alist-get 0.5 ii/vertico--buffer-width-alist nil nil 'eql))) 0.5)
                        ((memq command (car (alist-get 0.35 ii/vertico--buffer-width-alist nil nil 'eql))) 0.25)
                        (t 0.5)))
           (side (if (or (>= (window-left-column (selected-window)) (/ (1- (frame-width)) 2))
                         (equal width 0.25))
                     'left
                   'right)))
      (message "width %S side %S si-minibufer %S" width side (minibufferp))
      `( :width ,width
         :height 1.0
         :side ,side))))

(defun ii/vertico--get-buffer-alist (command)
  (let ((props (ii/vertico--buffer-get-display-alist-props (>= (frame-pixel-width) (frame-pixel-height))
                                                           command)))
    `((window-width . ,(plist-get props :width))
      (inhibit-same-window . t)
      (window-height . ,(plist-get props :height))
      (side . ,(plist-get props :side))
      (slot . 1))))

;;;###autoload
(defun ii/vertico--buffer-mode-display-buffer (buffer alist)
  "Display BUFFER in side window, hiding other windows but the selected one."
  (setq ii/vertico--window-config (current-window-configuration))
  (when (not (one-window-p)) (delete-other-windows))
  (add-hook 'minibuffer-exit-hook 'ii/vertico--restore-window-config)
  (cond ((with-current-buffer (window-buffer (minibuffer-selected-window))
           (eq major-mode 'vertico-buffer-mode))
         (display-buffer-same-window buffer alist))
        (t (display-buffer-in-side-window buffer (append (ii/vertico--get-buffer-alist this-command) alist)))))

(defvar ii/vertico-embark-select-overlays nil
  "TODO")

(defun ii/vertico-embark-mode-select (arg)
  (interactive "p")
  (let ((arg (if (not (numberp arg)) 1 arg)))
    (dotimes (i (abs arg))
      (embark-select)
      (vertico-previous (if (< 0 arg) -1 1)))))

;;;###autoload
(defun ii/vertico-embark-noop ()
  (interactive))

;;;; modal navigation in vertico minibuffers
(defun ii/vertico-embark-setup-minibuffer-keys ()
  (let ((map (current-local-map)))
    (bind-key [remap self-insert-command] #'ii/vertico-embark-noop map)
    (keymap-local-set "m" #'ii/vertico-embark-mode-select)
    (keymap-local-set "a" #'embark-act)
    (keymap-local-set "j" #'vertico-next)
    (keymap-local-set "J" #'vertico-next-group)
    (keymap-local-set "k" #'vertico-previous)
    (keymap-local-set "K" #'vertico-previous-group)
    (keymap-local-set "A" #'embark-act-all)
    (keymap-local-set "i" #'ii/vertico-embark-mode)
    (keymap-local-set "ESC" #'meow-minibuffer-quit)
    (keymap-local-set "<escape>" #'meow-minibuffer-quit)))

(defun ii/vertico-embark-reset-minibuffer-keys ()
  (let ((map (current-local-map)))
    ;; needs to be nil since we are resetting existing remapping
    (bind-key [remap self-insert-command] nil map)
    (keymap-local-set "m" #'self-insert-command)
    (keymap-local-set "SPC" #'self-insert-command)
    (keymap-local-set "a" #'self-insert-command)
    (keymap-local-set "j" #'self-insert-command)
    (keymap-local-set "i" #'self-insert-command)
    (keymap-local-set "J" #'self-insert-command)
    (keymap-local-set "k" #'self-insert-command)
    (keymap-local-set "K" #'self-insert-command)
    (keymap-local-set "A" #'self-insert-command)
    (keymap-local-set "ESC" #'ii/vertico-embark-mode)
    (keymap-local-set "<escape>" #'ii/vertico-embark-mode)))

(defvar-keymap ii/vertico-embark-mode-map
  :doc "Map for vertico-mebark-mode"
  :parent vertico-map
  "<remap> <self-insert-command>" #'ii/vertico-embark-noop
  "m" #'ii/vertico-embark-mode-select
  "a" #'embark-act
  "j" #'vertico-next
  "J" #'vertico-next-group
  "TAB" #'vertico-next-group
  "k" #'vertico-previous
  "K" #'vertico-previous-group
  "<backtab>" #'vertico-previous-group
  "A" #'embark-act-all
  "i" #'ii/vertico-embark-mode-silent
  "ESC" #'meow-minibuffer-quit
  "<escape>" #'meow-minibuffer-quit)

(defun ii/vertico-embark-exit-minibuffer ()
  (ii/vertico-embark-reset-minibuffer-keys))

(defun ii/vertico-embark-mode--turn-on ()
  (setq-local embark-quit-after-action nil)
  (let ((inhibit-message t))
    (message "ve on"))
  ;; (ii/vertico-embark-setup-minibuffer-keys)
  (add-hook 'minibuffer-exit-hook 'ii/vertico-embark-exit-minibuffer nil t))

(defun ii/vertico-embark-mode--turn-off ()
  (setq-local embark-quit-after-action t)
  (let ((inhibit-message t))
    (message "ve off"))
  ;; (ii/vertico-embark-reset-minibuffer-keys)
  (remove-hook 'minibuffer-exit-hook 'ii/vertico-embark-exit-minibuffer t))

;;;###autoload
(define-minor-mode ii/vertico-embark-mode
  ""
  :lighter "VE"
  :keymap ii/vertico-embark-mode-map
  (if ii/vertico-embark-mode
      (ii/vertico-embark-mode--turn-on)
    (ii/vertico-embark-mode--turn-off)))

(defun ii/vertico-embark-mode-silent (&optional arg)
  (let ((inhibit-message t))
    (ii/vertico-embark-mode arg)))

(provide 'implicit-vertico)

;;; implicit-vertico.el ends here
