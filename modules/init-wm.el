;;; init-wm.el --- emacs window manager based on i3 -*- lexical-binding: t -*-

;; Author: Błażej Niewiadomski
;; Maintainer: Błażej Niewiadomski
;; Version: 0.1
;; Package-Requires: nil
;; Homepage:
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
;; This programs allows to dynamically change where the next Emacs window will be displayed

;; commentary

;;; Code:



(defvar ewm-mode-hook '())

(defvar ewm-config-symbols '())

(defmacro ewm-define-config-symbol (symbol))

(defmacro ewm-window-cfg (&rest cfg-forms)
  "Each of CFG-FORMS is alist of (MODES-OR-BUFFER-NAMES . EWM-CONFIG-SYMBOLS) where
each of `EWM-CONFIG-SYMBOLS' are one of keys of `ewm-config-symbols'")

(defvar ewm-action-alist
  (list :split-left '((display-buffer-in-side-window)
                      (side . left))
        :split-right '((display-buffer-in-side-window)
                       (side . right))))



(defun ewm-modeline-segment ()
  "")

(with-eval-after-load 'mood-line
  (defun ewm-mood-line-segment ()
    ""))


(define-minor-mode ewm-mode
  "Minor mode for managing windows in EMACS."
  :global t
  :init-value nil
  :lighter "ewm"
  (if ewm-mode
      (run-hooks ewm-mode-hook)))





;;; name.el ends here

(provide 'init-wm)
