;;; yuck-mode.el --- Major mode for yuck, configuration language for eww widgets -*- lexical-binding: t -*-

;; Author: Błażej Niewiadomski
;; Maintainer: Błażej Niewiadomski
;; Version: 0.1
;; Package-Requires: (dependencies)
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

;;; Code:
(defgroup yuck nil
  "yuck major mode."
  :group 'languages)

(eval-when-compile
  (defvar yuck-keywords-list
    '("defvar" "defpoll" "deflisten" "defwindow" "defwidget" "for" "include"))

  (defvar yuck-widgets-list
    '("combo-box-text"
      "expander" "revealer" "checkbox" "color-button" "color-chooser" "scale" "progress"
      "image" "button" "inp" "box" "overlay" "centerbox" "eventbox" "scroll"
      "label" "literal" "transform" "calendar" "circular-progress" "graph" "geometry"))

  (defun yuck-ppre (re)
    (format "\\_<\\(%s\\)\\>" (regexp-opt re))))

(defvar yuck-font-lock-keywords
  (list
   ;; keys - e.g :path, :class, etc.
   (cons ":[a-z-]*"
         font-lock-builtin-face)
   ;; keywords
   (cons (eval-when-compile
           (yuck-ppre yuck-keywords-list))
         font-lock-keyword-face)
   ;; types
   (cons (eval-when-compile
           (yuck-ppre yuck-widgets-list))
         font-lock-type-face)))

(defconst yuck-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; "" - string delimiter
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\` "\"" table)

    ;; ";" - comment start
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)

    (modify-syntax-entry ?\: "'" table)

    ;; open and closing parens
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)

    ;; open and closing braces
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    ;; open and closing curly braces
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    table))

;;;###autoload
(define-derived-mode yuck-mode prog-mode "Yuck"
  "Major mode for editing yuck config files."
  :syntax-table yuck-mode-syntax-table
  (setq-local font-lock-defaults '(yuck-font-lock-keywords))
  (setq-local indent-line-function #'lisp-indent-line)
  (setq-local comment-start ";; ")
  (setq-local comment-end "")
  (setq-local comment-padding "")
  (font-lock-ensure))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.yuck\\'" . yuck-mode))


(provide 'yuck-mode)

;;; yuck-mode.el ends here
