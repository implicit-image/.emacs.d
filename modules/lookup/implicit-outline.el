;; -*- lexical-binding: t; byte-compile-warnings: nil  -*-


(require 'outline)

;;; Code:

(defvar ii/outline--default-face-spec '()
  "Default face spec for `outline-*' faces.")

(defvar ii/outline-minor-mode-ellipsis " ...v "
  "String used for hidden outline entries.")

(defvar ii/outline-minor-mode-max-level-alist '((python-ts-mode . 2)
                                                (emacs-lisp-mode . 8)
                                                (nwscript-mode . 6))
  "Alist mapping max outline entry level to highlight in `outline-minor-mode'.")

(defun ii/outline-minor-mode--setup-highlight (max-level)
  "Setup faces for outline entries up to MAX-LEVEL."
  (let* ((bg-color (or (doom-color 'bg-alt)
                       "#181818"))
         (top-level-spec `(:background ,bg-color :foreground unspecified :weight ,(face-attribute 'default :weight) :extend t :inherit default))
         (low-level-spec `(:background unspecified :foreground unspecified :weight ,(face-attribute 'default :weight) :extend nil :inherit default))
         (args '(outline-1
                 outline-2
                 outline-3
                 outline-4
                 outline-5
                 outline-6
                 outline-7
                 outline-8))
         (top-args (take max-level args))
         (low-args (drop max-level args)))
    (dolist (face top-args)
      (face-remap-add-relative face
                               top-level-spec))
    (dolist (face low-args)
      (face-remap-add-relative face
                               low-level-spec))))

(defun ii/outline-minor-mode--get-max-level (major-mode)
  "Get max level outline highlight level for MAJOR-MODE."
  (or (alist-get major-mode ii/outline-minor-mode-max-level-alist)
      8))

(defun ii/outline-minor-mode--set-elipsis (ellipsis)
  "Set ellipsis used to denote hidden entries to ELLIPSIS."
  (let* ((dtable (or buffer-display-table (make-display-table)))
         (face-offset (* (face-id 'shadow) (ash 1 22)))
         (value (vconcat (mapcar (lambda (c)
                                   (+ face-offset c))
                                 (string-trim-right ellipsis)))))
    (set-display-table-slot dtable 'selective-display value)
    (setq buffer-display-table dtable)))

(defun ii/outline-minor-mode--setup ()
  "Setup outline minor mode local buffer settings."
  (ii/outline-minor-mode--set-elipsis ii/outline-minor-mode-ellipsis)
  (let ((max-level (ii/outline-minor-mode--get-max-level major-mode)))
    (when (and (> max-level 0)
               outline-minor-mode-highlight)
      (ii/outline-minor-mode--setup-highlight max-level))))


(provide 'implicit-outline)
