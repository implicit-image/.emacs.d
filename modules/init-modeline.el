;;; -*- lexical-binding: t -*-

;; (use-package emacs
;;   :init

;;; Code:


(defmacro +defconstruct! (name &rest body)
  "Define mode-line construct NAME with BODY."
  (declare (indent defun))
  (let ((name (intern (concat "+mode-line-" (symbol-name name)))))
    `(progn (defvar-local ,name
                ,@body)
            (put ',name 'risky-local-variable t))))

(+defconstruct! modal
  '(:eval (propertize (cond (evil-mode evil-mode-line-tag)
                            (meow-global-mode meow--indicator))
                      'face `(:background ,(doom-color 'yellow)))))

(+defconstruct! buffer-directory
  '(:propertize (:eval (or (projectile-project-name)
                           default-directory))
                'face 'success))

(+defconstruct! flyspell
  '(flyspell-mode flyspell-mode-line))

(+defconstruct! position
  "L%l:%C")

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                +mode-line-modal
                +mode-line-buffer-status
                (:propertize
                 (""
                  "%*"
                  "%@")
                 (min-width (6.0)))
                +mode-line-buffer-directory
                "/"
                mode-line-buffer-identification
                " "
                +mode-line-position
                " "
                "%o"
                mode-line-format-right-align
                (vc-mode vc-mode)
                " "
                ;; checkers
                " "
                +mode-line-flyspell
                " "
                mode-name
                mode-line-misc-info
                mode-line-end-spaces
                " "))

;; (use-package mood-line
;;   :custom-face
;;   (mood-line-unimportant ((t (:foreground ,(doom-color 'base6)))))
;;   :init
;;   (defun +mood-line-evil-mc-segment ()
;;     ""
;;     (when (> (evil-mc-get-cursor-count) 1)
;;       (evil-mc-active-mode-line nil)))
;;
;;   (defun +mood-line--setup ()
;;     (require 'mood-line)
;;     (require 'mood-line-segment-checker)
;;     (require 'mood-line-segment-vc)
;;     (require 'mood-line-segment-indentation)
;;     (setq-default mood-line-format
;;                   (mood-line-defformat
;;                    :left
;;                    (((mood-line-segment-modal) . " ")
;;                     ((mood-line-segment-region) . " ")
;;                     ((+mood-line-evil-mc-segment) . " ")
;;                     ((mood-line-segment-anzu) . " ")
;;                     ((propertize (mood-line-segment-project)
;;                                  'face 'mood-line-status-success)
;;                      . "/")
;;                     ((mood-line-segment-buffer-status) . ":")
;;                     ((mood-line-segment-buffer-name) . " ")
;;                     ((format-mode-line "L%l:%C") . " ")
;;                     ((format-mode-line "%o") . " "))
;;                    :right
;;                    (((mood-line-segment-misc-info) . " ")
;;                     ((mood-line-segment-checker) . " ")
;;                     ((mood-line-segment-vc) . " ")
;;                     ((mood-line-segment-major-mode) . " "))))
;;     (mood-line-mode 1))
;;   :hook
;;   (window-setup-hook . +mood-line--setup))


(provide 'init-modeline)
