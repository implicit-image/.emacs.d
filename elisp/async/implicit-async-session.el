;;; implicit-async-session.el --- Emacs deamon subprocess management -*- lexical-binding: t -*-

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

(eval-when-compile (require 'org))
(eval-when-compile (require 'dash))

(declare-function async-inject-variables "async")
(declare-function async-inject-environment "async")
(declare-function async-start "async")



(defcustom ii/async-min-free-process 1
  "Minimal number of free processes at any given time.")

(defvar ii/async-process-pool nil
  "Currently avaiable async processes.")

(defvar ii/async-program "emacs"
  "Program name for emacs.")

(defun ii/async--find-free-process ()
  (seq-find (lambda (proc-plist)
              (let ((proc (plist-get proc-plist :process)))
                (when (and (process-live-p proc)
                           (process-s)))))
            ii/async-process-pool))

(defun ii/async-emacs-deamon-args (id)
  (list (format "--deamon=emacs-proc-%s" id)))

(defun ii/async-start-deamon ()
  (let ((proc (apply #'start-process
                     "emacs-deamon"
                     (generate-new-buffer "emacs-deamon")
                     ii/async-program
                     `(,@(ii/async-emacs-deamon-args)))))))

(defun ii/async-kill-all ()
  (mapc (lambda (proc)
          (when (and (processp proc) (process-live-p proc))
            (kill-process proc)))
        ii/async-process-pool)
  (setq ii/async-process-pool nil))

(defun ii/async-send-sexp (sexp proc)
  "Send SEXP to emacs process PROC."
  (cond
   ((process-live-p proc))
   (process -p$))
  (let ((p-name (process-name proc)))))

(provide 'implicit-async-session)
;;; implicit-async-session.el ends here
