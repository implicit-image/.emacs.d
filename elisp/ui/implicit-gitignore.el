;;; implicit-gitignore.el --- gitignore utils -*- lexical-binding: t -*-

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

(defvar ii/gitignore-count-command "find . -path ./.git -prune -o -print | git check-ignore --no-index --stdin --verbose | cut -f1 | sort | uniq -c")

(defvar ii/gitignore-check-command "git status --porcelain | grep .gitignore")

(defvar-local ii/gitignore-count-info nil)

(defvar-local ii/gitignore-overlay-list nil)

(defvar-keymap ii/gitignore-count-mode-map
  :doc "Keymap for `ii/gitignore-count-map'."
  "C-c C-r" #'ii/gitignore-count-matches
  "C-c C-t" #'ii/gitignore-toggle-overlays)

(defface ii/gitignore-count-face
  '((t :inherit font-lock-doc-face
       :slant italic))
  "Face for gitignore counters."
  :group 'gitignore-lens)

(defun ii/gitignore--should-recount (buffer)
  "Return non-nil if gitignore file count in BUFFER should be recounted, nil otherwise."
  (with-current-buffer buffer
    (let ((default-directory (or (project-root (project-current nil))
                                 default-directory))
          (res (not (string-empty-p (shell-command-to-string ii/gitignore-check-command)))))
      (progn (message "should recount is %S" res)
             (or (not ii/gitignore-count-info) res)))))

(defun ii/gitignore-count-available ()
  "Check if counting gitignore files is supported."
  (and (executable-find "cut")
       (executable-find "find")
       (executable-find "uniq")
       (executable-find "git")))

(defun ii/gitignore-make-overlay (pos count max-len)
  (let* ((curr-len (- pos (pos-bol)))
         (ov (make-overlay pos (1+ pos) (current-buffer) t)))
    (overlay-put ov 'face `ii/gitignore-count-face)
    (overlay-put ov 'display (concat (make-string (+ 5 (- max-len curr-len)) 32) " " count " \n"))
    ;; (overlay-put ov 'before-string (make-string (+ 5 (- max-len curr-len)) 32))
    (overlay-put ov 'gitignore-count count)
    ;; (overlay-put ov 'after-string "\n")
    ov))

(defun ii/gitignore-toggle-overlays ()
  (interactive)
  (if (null ii/gitignore-count-info)
      (ii/gitignore-count-matches)
    (if ii/gitignore-overlay-list
        (ii/gitignore-clear-overlays (current-buffer))
      (ii/gitignore-annotate-patterns ii/gitignore-count-info))))

(defun ii/gitignore-longest-pattern-length (info)
  (car (seq-sort #'> (mapcar (lambda (el) (length (nth 0 el)))
                             info))))

(defun ii/gitignore-clear-overlays (buf)
  "Delete all gitignore overlays in BUF."
  (with-current-buffer buf
    (save-mark-and-excursion
      (widen)
      (dolist (ov ii/gitignore-overlay-list)
        (when (overlay-get ov 'gitignore-count)
          (delete-overlay ov)))
      (setq-local ii/gitignore-overlay-list nil))))

(defun ii/gitignore-annotate-patterns (info)
  (save-mark-and-excursion
    (widen)
    (beginning-of-buffer)
    (let ((maxlen (ii/gitignore-longest-pattern-length info)))
      (dolist (match info)
        (when match
          (let* ((pat (nth 0 match))
                 (line (nth 1 match))
                 (count (nth 2 match)))
            (goto-line (string-to-number line))
            (add-to-list 'ii/gitignore-overlay-list (ii/gitignore-make-overlay (pos-eol) count maxlen))))))))

(defun ii/gitignore-revert-function (&rest _args)
  (ii/gitignore-clear-overlays (current-buffer)))

(defun ii/gitignore-count-matches ()
  (interactive)
  (let ((buf (current-buffer)))
    (when (ii/gitignore--should-recount buf)
      (ii/gitignore-clear-overlays buf)
      (async-start
       (lambda ()
         (let ((counts (mapcar (lambda (s)
                                 (let* ((pair (string-split (string-trim s) " " t)))
                                   (when (length> pair 1)
                                     (string-match ".gitignore:\\([0-9]+\\):\\(.*\\)" (nth 1 pair))
                                     (list (match-string 2 (nth 1 pair)) (match-string 1 (nth 1 pair)) (nth 0 pair)))))
                               (string-lines (shell-command-to-string "find . -path ./.git -prune -o -print | git check-ignore --no-index --stdin --verbose | cut -f1 | sort | uniq -c")))))
           counts))
       (lambda (res)
         (with-current-buffer buf
           (when res
             (setq-local ii/gitignore-count-info res)
             (ii/gitignore-annotate-patterns res))))))))

;;;###autoload
(define-minor-mode ii/gitignore-count-mode
  "Minor mode for displaying the number of ignored files."
  :keymap ii/gitignore-count-mode-map
  (if ii/gitignore-count-mode
      (progn (ii/gitignore-count-matches)
             (add-hook 'after-save-hook #'ii/gitignore-count-matches nil t)
             (add-hook 'revert-buffer-restore-functions #'ii/gitignore-revert-function nil t))
    (remove-hook 'after-save-hook #'ii/gitignore-count-matches t)
    (remove-hook 'revert-buffer-restore-functions #'ii/gitignore-revert-function t)))

(provide 'implicit-gitignore)

;;; implicit-gitignore.el ends here
