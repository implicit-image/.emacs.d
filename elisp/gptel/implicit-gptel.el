;;; implicit-gptel.el --- gptel utils -*- lexical-binding: t -*-

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

(require 'gptel)
(require 'gptel-agent)
(require 'implicit-ui)

(defun ii/gptel-insert-response--retry (response info &optional retry)
  (ii/ui-delete-confirmation-overlay ov))

(defun ii/gptel-insert-response (prompt &optional retry)
  (interactive (list (read-string "Prompt: ") current-prefix-arg))
  (let* ((ov (ii/ui-make-placeholder-overlay (concat (symbol-name gptel-model))
                                             prompt
                                             ""
                                             'success
                                             (lambda (ov)
                                               (let ((val (pcase (string-trim (overlay-get ov 'after-string))
                                                            ("|" "||")
                                                            ("||" "|||")
                                                            ("|||" "||||")
                                                            (_ "|"))))
                                                 (overlay-put ov 'after-string (concat val "\n"))))
                                             0.3))
         (gptel-include-reasoning nil))
    (gptel-request prompt
      :system (concat (when retry "Try again! ")
                      "Answer extremely directly and in minimal number of lines.Dont use markdown blocks for code. No yapping!")
      :stream nil
      :callback (lambda (response info)
                  (with-current-buffer (plist-get info :buffer)
                    (save-mark-and-excursion
                      (ii/with-no-message!
                       (message "gptel response is %S of type %S" response (type-of response))
                       (message "info is %S" info))
                      (goto-char (plist-get info :position))
                      (ii/ui-delete-placeholder-overlay ov)
                      (cond ((stringp response)
                             (ii/ui-make-confirmation-overlay response
                                                              (lambda (ov)
                                                                (message "Confirmed!")
                                                                (save-mark-and-excursion
                                                                  (let ((content (overlay-get ov 'ii/ui-content)))
                                                                    (goto-char (overlay-start ov))
                                                                    (ii/ui-delete-confirmation-overlay ov)
                                                                    (insert content))))
                                                              (lambda (ov)
                                                                (message "Retrying!")
                                                                (goto-char (overlay-start ov))
                                                                (ii/ui-delete-confirmation-overlay)
                                                                (let ((prompt (plist-get (elt (plist-get info :messages) 0) :content)))
                                                                  (ii/gptel-insert-response prompt t)))))
                            (t (message "Parsing response failed")))))))))

(provide 'implicit-gptel)

;;; implicit-gptel.el ends here
