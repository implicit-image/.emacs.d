;;; -*- lexical-binding: t -*-

;;; Code:

(defvar +mode-line--vc-text "")

(defvar +mode-line--lsp-text "")

(defmacro +defconstruct! (name &rest body)
  "Define mode-line construct +mode-line-NAME with BODY."
  (declare (indent defun))
  (let ((name (intern (concat "+mode-line-" (symbol-name name)))))
    `(progn (defvar-local ,name
                ,@body)
            (put ',name 'risky-local-variable t))))

(+defconstruct! modal
  '(:eval meow--indicator))

(+defconstruct! buffer-directory
  '(:propertize (:eval
                 (or (and (fboundp 'project-name)
                          (project-current)
                          (concat (project-name (project-current))
                                  "/"))
                     default-directory))
                face success))

(+defconstruct! flyspell
  '(flyspell-mode flyspell-mode-line))

(+defconstruct! position
  '(:eval "L%l:%C"))

(add-hook 'find-file-hook #'+mode-line--vc-update)
(add-hook 'after-save-hook #'+mode-line--vc-update)
(advice-add #'vc-refresh-state :after #'+mode-line--vc-update)

(add-hook 'pdf-view-mode-hook '+mode-line--pdf-update)
(add-hook 'pdf-view-change-page-hook '+mode-line--pdf-update)

(+defconstruct! vc
  '(:eval +mode-line--vc-text))

(+defconstruct! flymake
  '(flymake-mode flymake-mode-line-counters))

(+defconstruct! compilation
  '(compilation-mode compilation-mode-line-errors))

(+defconstruct! buffer-status
  '(:eval
    (propertize (if (or buffer-file-read-only
                        (buffer-modified-p))
                    "%*"
                  "")
                'face 'error)
    (propertize (if (file-remote-p)
                    "%@"
                  ""))))

(+defconstruct! major-mode
  '(:eval (:propertize mode-name
                       face bold)))

(setq-default mode-line-format
              '("%e"
                " "
                +mode-line-modal
                " "
                +mode-line-buffer-directory
                +mode-line-buffer-status
                "%["
                mode-line-buffer-identification
                "%]"
                " "
                +mode-line-position
                " "
                "%o"
                " "
                mode-line-format-right-align
                (text-scale-mode (:eval (concat  "[" text-scale-mode-lighter "] ")))
                (pdf-view-mode (:eval ))
                +mode-line-flymake
                " "
                +mode-line-vc
                " "
                (:propertize mode-name face bold)
                mode-line-process
                " "))

(provide 'init-modeline)
