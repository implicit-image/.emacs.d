;;; -*- lexical-binding: t -*-

(defvar-local 2da-columns '()
  "")

(defvar-local 2da-row-template ""
  "")

(defvar-local 2da-row-id 0
  "")

(defun 2da-add-entry ()
  "")

(defun 2da-next-column ()
  "")

(defun 2da-prev-column ()
  "")

(defun 2da-goto-column ()
  "")

(defun 2da-get-row-id-at-point ()
  "")


(define-derived-mode 2da-mode csv-mode "2da"
  "Mode for editing .2da files.")


(add-to-list 'auto-mode-alist '("\\.2da\\'" . 2da-mode))
(add-to-list 'auto-mode-alist '("\\.2DA\\'" . 2da-mode))
(provide 'init-2da)
