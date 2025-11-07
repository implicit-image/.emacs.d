;;; -*- lexical-binding: t -*-
(require 'window)
(require 'windmove)

(defvar +windows--last-buffer nil)

(defvar +windows--generic-ring (make-ring 16))

(defvar +windows--dedicated-ring (make-ring 16))

(defvar +windows--file-buffer-ring (make-ring 16))

(defvar +windows--special-buffer-ring (make-ring 16))

(defvar window-track-mode-hook nil)

(defvar-local +windows--saved-mode-line-format nil)

(defvar +windows--window-choice-alist '((?d "Dedicated")
                                        (?w "Generic")
                                        (?f "File")
                                        (?s "Special")))

(defvar +windows--window-type-alist '((?d . +windows--show-last-dedicated-window)
                                      (?w . +windows--show-last-generic-buffer)
                                      (?f . +windows--show-last-file-buffer)
                                      (?s . +windows--show-last-special-buffer)))

(defvar +windows--maximize-saved-config nil
  "Saved config before maximizing window")

(defvar +windows-display-buffer-prefix-hook nil
  "Hook run every time a prefix command is used.")

(defvar +windows--quick-action-alist '((?r )))

;;;; Macros

(defmacro +define-display-buffer-prefix! (symbol display-fn echo &optional bonus-alist &rest body)
  "Define SYMBOL as display buffer prefix command in the style of `other-window-prefix' but using display function\
DISPLAY-FN and showing echo message ECHO. If BONUS-ALIST is non-nil, it is appended to alist passed to DISPLAY-FN.\
If BODY is non-nil, it is executed before returning window."
  (declare (indent defun))
  `(defun ,symbol nil
     (interactive)
     (display-buffer-override-next-command
      (lambda (buffer alist)
        (let ((alist (append '((inhbit-same-window . t))
                             ',bonus-alist
                             alist))
              window
              type)
          (setq window (,display-fn buffer alist)
                type 'reuse)
          ,@body
          (cons window type)))
      nil ,echo)))

;;;; Functions
(defun +windows--hide-modeline ()
  "Temporarily set local `mode-line-format' to nil."
  (setq-local +windows--saved-mode-line-format mode-line-format
              mode-line-format nil))

(defun +windows--show-modeline ()
  (when +windows--saved-mode-line-format
    (setq-local mode-line-format +windows--saved-mode-line-format)))

(defun +windows/display-buffer-in-left-vsplit (buffer alist)
  (let ((window (split-window-horizontally)))
    (when window
      (window--display-buffer buffer (window-in-direction 'left) 'reuse alist))))

(defun +windows/display-buffer-in-right-vsplit (buffer alist)
  (let ((window (split-window-horizontally)))
    (when window
      (window--display-buffer buffer (window-in-direction 'right) 'reuse alist))))

(defun +windows/display-buffer-in-top-hsplit (buffer alist)
  (let ((window (split-window-vertically)))
    (when window
      (window--display-buffer buffer (window-in-direction 'above) 'reuse alist))))

(defun +windows/display-buffer-in-bot-hsplit (buffer alist)
  (let ((window (split-window-vertically)))
    (when window
      (window--display-buffer buffer (window-in-direction 'below) 'reuse alist))))

(+define-display-buffer-prefix! +windows/left-vsplit-prefix
                                +windows/display-buffer-in-left-vsplit
                                "[vsplit-left]")

(+define-display-buffer-prefix! +windows/right-vsplit-prefix
                                +windows/display-buffer-in-right-vsplit
                                "[vsplit-right]")

(+define-display-buffer-prefix! +windows/above-hsplit-prefix
                                +windows/display-buffer-in-top-hsplit
                                "[hsplit-top]")

(+define-display-buffer-prefix! +windows/below-hsplit-prefix
                                +windows/display-buffer-in-bot-hsplit
                                "[hsplit-bot]")

(+define-display-buffer-prefix! +windows/below-selected-prefix
                                display-buffer-below-selected
                                "[below-selected]"
                                ((window-height . 0.5)))

(+define-display-buffer-prefix! +windows/bottom-window-prefix
                                display-buffer-at-bottom
                                "[at-bottom-window]"
                                ((window-height . 0.3)))

(+define-display-buffer-prefix! +windows/right-side-window-prefix
                                display-buffer-in-side-window
                                "[in-right-side-window]"
                                ((window-width . 0.5)
                                 (side . right)
                                 (slot . 1)))

(+define-display-buffer-prefix! +windows/left-side-window-prefix
                                display-buffer-in-side-window
                                "[in-left-side-window]"
                                ((window-width . 0.5)
                                 (side . left)
                                 (slot . 1)))

(+define-display-buffer-prefix! +windows/top-side-window-prefix
                                display-buffer-in-side-window
                                "[in-top-side-window]"
                                ((window-height . 0.5)
                                 (side . top)
                                 (slot . 1)))

(+define-display-buffer-prefix! +windows/bottom-side-window-prefix
                                display-buffer-in-side-window
                                "[in-botoom-side-window]"
                                ((window-height . 0.5)
                                 (side . bottom)
                                 (slot . 1)))

(defun +windows/toggle-modeline (buffer)
  "Toggle modeline visibility in BUFFER."
  (interactive (list (current-buffer)))
  (with-current-buffer buffer
    (if mode-line-format
        (+windows--hide-modeline)
      (+windows--show-modeline))))

(defun +windows/delete-in-direction (this-window dir)
  "Delete the window in direction DIR from window THIS-WINDOW."
  (interactive) (list (selected-window)
                      ()))

(defun +windows/toggle-maximize-window (this-window frame)
  "Maximize window THIS-WINDOW if it is not maximized, else restore window configuration before last\
maximization."
  (interactive (list (selected-window)
                     (selected-frame)))
  ;; check if there are other windows
  (if (one-window-p)
      (if (not +windows--maximize-saved-config)
          (message "%S is the only window." this-window)
        (set-window-configuration +windows--maximize-saved-config)
        (setq +windows--maximize-saved-config nil))
    (setq +windows--maximize-saved-config (current-window-configuration frame))
    (delete-other-windows this-window)))

(defun +windows/rotate (backwards)
  "Rotate window configuration on current frame forward. If BACKWARDS is \
non-nil, rotate backwards instead."
  (interactive))

(provide 'implicit-windows)
