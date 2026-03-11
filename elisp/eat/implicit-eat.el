;;; implicit-eat.el --- eat utilities -*- lexical-binding: t -*-

;; Author: Błażej Niewiadomski
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

(require 'eat)

(defvar-local ii/eat-current-frame nil)

(defun ii/eat--get-buffer (id)
  "ID."
  (let* ((name (format "eat[%s]" id))
         (buf (get-buffer name)))
    (if (process-live-p (get-buffer-process buf))
        buf
      (get-buffer-create name))))

(defun ii/eat--initialize-process (program &rest args)
  (let ((buf (ii/eat--get-buffer program)))
    (with-current-buffer buf
      ;; (when (eq major-mode #'eat-mode)
      ;;   (read-only-mode -1)
      ;;   (erase-buffer)
      ;;   (read-only-mode 1))
      (unless (eq major-mode #'eat-mode)
        (eat-mode))
      (when (bound-and-true-p whitespace-mode)
        (whitespace-mode -1))
      (unless (and eat-terminal
                   (eat-term-parameter eat-terminal 'eat--process))
        (eat-exec buf (buffer-name) "/usr/bin/env" nil
                  `("sh" "-c" ,(shell-quote-argument (format "%s %s" program (string-join args " "))))))
      (if-let* ((proc (get-buffer-process buf)))
          (progn (message "buffer process is %S" proc)
                 (set-process-sentinel proc (lambda (proc change)
                                              (message "received %s in sentinel" change)
                                              (when  (process-live-p proc)
                                                (message "process is still live")))))
        (message "process is dead")
        proc))))

(defun ii/eat-edit-with-nvim (file)
  (let ((proc (ii/eat--initialize-process "nvim" file)))
    (display-buffer (process-buffer proc))))

(ii/eat-edit-with-nvim (expand-file-name "~/test.c"))


(provide 'implicit-eat)
;;; implicit-eat.el ends here
