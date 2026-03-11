;;; implicit-meow-shell-command.el --- summary -*- lexical-binding: t -*-

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
(require 'implicit-ui)

(defun ii/meow-insert-async-shell-command (cmd)
  (interactive (list (read-shell-command "Run: ")))
  (let* ((ov (ii/ui-make-placeholder-overlay (propertize (format "Running %s" cmd) 'face 'error)
                                             cmd
                                             "Waiting"
                                             'success
                                             #'ii/ui-placeholder-animate-elipsis
                                             0.5)))
    (async-start
     (lambda ()
       (shell-command-to-string cmd))
     (lambda (result)
       (with-current-buffer (overlay-buffer ov)
         (save-mark-and-excursion
           (ii/with-no-message!
            (message "result is %S, type is %S" result (type-of result)))
           (goto-char (overlay-start ov))
           (ii/ui-delete-placeholder-overlay ov)
           (ii/ui-make-confirmation-overlay result
                                            (lambda (ov)
                                              (message "Confirmed!")
                                              (save-mark-and-excursion
                                                (let ((content (overlay-get ov 'ii/ui-content)))
                                                  (goto-char (overlay-start ov))
                                                  (ii/ui-delete-confirmation-overlay ov)
                                                  (insert content)))))))))))



(provide 'implicit-meow-shell-command)

;;; implicit-meow-shell-command.el ends here
