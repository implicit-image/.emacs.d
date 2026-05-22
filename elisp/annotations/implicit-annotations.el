;;; implicit-annotations.el --- File annotations -*- lexical-binding: t -*-

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcustom ii/annot-db-file (expand-file-name "ii-note-db.eld" user-emacs-directory)
  "file for saving notes")

(defcustom ii/annot-exporter-alist nil
  "Alist of (SYMBOL . EXPORTER-FUNCTION) where the EXPORTER-FUNCTION is responsible for exporting the `annot-table' to some format.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface ii/annot-default-face
  '((t :inherit whitespace-missing-newline-at-eof))
  "Default face for note overlays.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ii/annot-table
  (make-hash-table :test 'equal :size 10))

(defvar ii/annot-db-loaded nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii/annot--overlay-p (ov)
  "Return non-nil if OV is an overlay storing note information, nil otherwise."
  (and (overlayp ov) (overlay-get ov 'ii/annot)))


(defun ii/annot--string-edit (prompt string setup-callback success-callback abort-callback read)
  "Modified `string-edit' with customized buffer setup function."
  (with-current-buffer (generate-new-buffer "*edit string*")
    (when prompt
      (let ((inhibit-read-only t))
        (insert prompt)
        (ensure-empty-lines 0)
        (add-text-properties (point-min) (point)
                             (list 'intangible t
                                   'face 'string-edit-prompt
                                   'read-only t))
        (insert (propertize (make-separator-line)
                            'read-only t 'rear-nonsticky t))
        (add-text-properties (point-min) (point)
                             (list 'string-edit--prompt t))))
    (let ((start (point)))
      (insert string)
      (goto-char start))

    ;; Use `fit-window-to-buffer' after the buffer is filled with text.
    (pop-to-buffer (current-buffer)
                   '(display-buffer-below-selected
                     (window-height . (lambda (window)
                                        (fit-window-to-buffer window nil 10)))))

    (set-buffer-modified-p nil)
    (setq buffer-undo-list nil)
    (funcall #'string-edit-mode)
    (string-edit-minor-mode)
    (funcall setup-callback (current-buffer))
    (setq-local string-edit--success-callback success-callback)
    (setq-local string-edit--abort-callback abort-callback)
    (setq-local string-edit--read read)
    (setq-local header-line-format
                (substitute-command-keys
                 "Type \\<string-edit-minor-mode-map>\\[string-edit-done] when you've finished editing or \\[string-edit-abort] to abort"))
    (message "%s" (substitute-command-keys
                   "Type \\<string-edit-minor-mode-map>\\[string-edit-done] when you've finished editing"))))

(defun ii/annot--prompt (note-ov)
  "Prompt for modifications to note stored in NOTE-OV."
  (when (ii/annot--overlay-p note-ov)
    (let ((content (overlay-get ov 'after-string))
          (file (buffer-file-name (overlay-buffer note-ov)))))))

(defun ii/annot--overlays-at (pos)
  "Get all note overlays at POS."
  (cl-remove-if-not
   (lambda (ov)
     (overlay-get ov 'ii/annot))
   (overlays-at pos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Storing notes in a table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii/annot--dump-valid-p (loaded-data)
  (when (listp loaded-data)
    (seq-every-p (lambda (entry)
                   (and (plistp entry)
                        (stringp (plist-get entry :file))
                        (listp (plist-get entry :notes))))
                 loaded-data)))

(defun ii/annot--add (file pos content)
  (let* ((info (gethash file ii/annot-table))
         (file-notes (plist-get info :notes)))
    (setq file-notes (push (cons pos content) file-notes))
    (setq info (plist-put info :notes file-notes))
    (puthash file info ii/annot-table)))

(defun ii/annot--remove (file pos)
  (let* ((info (gethash file ii/annot-table))
         (file-notes (plist-get info :notes)))
    (setq file-notes (cl-remove-if (lambda (note) (= (car note) pos)) file-notes))
    (setq info (plist-put info :notes file-notes))
    (puthash file info ii/annot-tale)))


(defun ii/annot--ensure-info-exists (file &optional set-info table)
  (let ((table (or table ii/annot-table)))
    (cond
     ((null (gethash file table))
      (when set-info
        (ii/annot--set-prop file :created time table))))))

(defun ii/annot--get-prop (file prop &optional table)
  "Get property PROP of annotation list for FILE in TABLE."
  (let* ((table (or table ii/annot-table))
         (info (gethash file table)))
    (plist-get info prop)))

(defun ii/annot--set-prop (file prop value &optional table)
  "Set property PROP for FILE to VALUE in TABLE."
  (let* ((table (or table ii/annot-table))
         (info (gethash file table)))
    (setq info (plist-put info prop value))
    (puthash file info table)))

(defun ii/annot--buffer-overlays (&optional buf)
  "Return all annotation overlays in BUF."
  (let ((buf (or buf (current-buffer))))
    (with-current-buffer buf
      (save-restriction
        (widen)
        (cl-remove-if-not (lambda (ov)
                            (overlay-get ov 'ii/annot))
                          (overlays-in (point-min) (point-max)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file hashing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii/annot--hash-buffer (buffer)
  "Return a secure, unique hash for contents of BUFFER."
  (secure-hash 'sha256 (buffer-substring-no-properties buffer)))

(defun ii/annot--set-file-hash (&optional buffer)
  "Set `:hash' property for file in BUFFER."
  (when-let* ((hash (ii/annot--hash-buffer (or buffer (current-buffer)))))
    (ii/annot--set-prop (buffer-file-name buffer) :hash hash)))

(defun ii/annot--set-last-modified (file)
  "Set `:last-modified' property for notes for FILE."
  (ii/annot--set-prop file :last-modified ))

(defun ii/annot--hash-match-p (buf-or-file)
  "Return non-nil if BUF-OR-FILE has been modified since last annot for it was saved."
  (cond
   ((and (bufferp buf-or-file) (null (buffer-modified-p buf-or-file)))
    (when-let* ((file (buffer-file-name buf-or-file))
                (hash (ii/annot--get-prop file :hash)))
      (eq hash (ii/annot--hash-buffer buf-or-file))))
   ((file-exists-p buf-or-file)
    (when-let* ((hash (ii/annot--get-prop buf-or-file :hash)))
      (with-temp-file buf-or-file
        (eq hash (ii/annot--hash-buffer (current-buffer))))))))

(defun ii/annot--verify-file-hash (buf)
  "Make sure that file in BUF was not changed since last note for it was saved."
  (if (or (ii/annot--hash-match-p buf) (y-or-n-p (format "File %s changed outside Emacs, load anyway?")))
      (ii/annot--set-prop (buffer-file-name buf) :unverified nil)
    ;; TODO: handle this
    (message "%S has been changed outside Emacs" (buffer-file-name buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; note loading and saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii/annot--load-from-file (&optional file)
  "Load saved data from FILE into `ii/annot-table'."
  (let ((file (or file ii/annot-db-file)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when-let* ((res (read (current-buffer)))
                  (o (ii/annot--dump-valid-p res)))
        (clrhash ii/annot-table)
        (dolist (entry res)
          (let ((file (car entry))
                (info (cdr entry)))
            (puthash file info ii/annot-table)
            (ii/annot--set-prop file :unverified t)
            (message "Loaded notes for %s: %S" file info)))))))

(defun ii/annot--serialize-table (&optional table)
  "Serialize TABLE into a list of expressions."
  (let ((table (or table ii/annot-table))
        (dump (list)))
    (dolist (key (hash-table-keys table) (nreverse dump))
      (when-let ((info (gethash key table)))
        (push (cons key info) dump)))))

(defun ii/annot--save-to-file (&optional note-table file)
  "Save annotations in NOTE-TABLE to FILE. If NOTE-TABLE is nil, it takes the value of `ii/annot-table'.
If FILE is nil, it takes the value of `ii/annot-db-file'."
  (let ((table (or note-table ii/annot-table))
        (file (or file ii/annot-db-file)))
    (when (and (hash-table-p table))
      (with-temp-buffer
        (let ((dump (ii/annot--serialize-table table)))
          (insert (prin1-to-string dump))
          (setq-local buffer-file-name file)
          (save-buffer))))))

;;;###autoload
(defun ii/annot-load-db ()
  "Load annotation save file from disk."
  (when (and (file-exists-p ii/annot-db-file) (null ii/annot-db-loaded))
    (when (ii/annot--load-from-file ii/annot-db-file)
      (setq ii/annot-db-loaded t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local ii/annot-display-style 'plain)

;;TODO: implement highlighting for links and files
(defun ii/annot--propertize-content (content)
  "Propertize CONTENT."
  content)

(defun ii/annot--make-overlay (beg content)
  "Create an overlay at BEG displaying CONTENT."
  (let ((ov (make-overlay beg (1+ beg)))
        (content (ii/annot--propertize-content content)))
    (overlay-put ov 'ii/annot t)
    (overlay-put ov 'after-string content)
    (overlay-put ov 'keymap ii/annot--overlay-map)
    (overlay-put ov 'face 'ii/annot-default-face)))

(defun ii/annot--render-overlays (buffer)
  "Create annotation overlays in BUFFER."
  (when-let* ((notes (ii/annot--get-prop (buffer-file-name buf) :notes)))
    (with-current-buffer buf
      (dolist (note notes)
        (let ((pos (car note))
              (text (cdr note)))
          (ii/annot--make-overlay pos text))))))

(defun ii/annot--remove-overlays (buffer)
  "Remove all annotation overlays in BUFFER."
  (with-current-buffer buf
    (remove-overlays (point-min) (point-max) 'ii/annot t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-keymap ii/annot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "C-c C-a" #'ii/annot-add)
    (define-key map "C-c C-d" #'ii/annot-remove)
    map))

(defvar-keymap ii/annot--overlay-map
  (let ((map (make-sparse-keymap)))
    (define-key map "RET" #'ii/annot-follow-links)
    map))

(defun ii/annot--before-save-hook ()
  "Function ran before saving a buffer in `ii/annot-mode'."
  (when (ii/annot--get-prop (buffer-file-name) :notes)
    (message "Saving file with notes.")))

(define-minor-mode ii/annot-mode
  "Minor mode for adding annotations for source files."
  (cond
   (ii/annot-mode
    (add-hook 'before-save-hook #'ii/annot--before-save-hook nil t))))


(defcustom ii/annot-autosave-interval 300
  "Number of seconds after which notes should be auto-saved again.")

(defvar ii/annot--autosave-timer nil)

(define-minor-mode ii/annot-autosave-mode
  "Global minor mode for autosaving notes."
  (cond
   (ii/annot-autosave-mode
    (let ((timer (run-with-idle-timer ii/annot-autosave-interval t #'ii/annot--save)))
      (when (timerp timer)
        (setq ii/annot--autosave-timer timer))))
   (t (when (timerp ii/annot--autosave-timer)
        (cancel-timer ii/annot--autosave-timer)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii/annot-add (pos text file)
  "Add annotation at POS in FILE with TEXT."
  (interactive (list (point) (read-string "Add: ") (buffer-file-name)))
  (when-let* ((text (string-trim-right text "[ \t\n]+"))
              (beg pos)
              (end (if (< (point-max) pos) ))
              (ov (make-overlay pos (1+ pos) )))))

(defun ii/annot-remove (pos text file))

(defun ii/annot-edit (pos)
  "Edit annotation at POS."
  (interactive (list (point)))
  (let ((ovs (ii/annot--overlays-at pos)))
    (cond
     ((null ovs)
      (message "No notes at point!"))
     ((length= ovs 1)
      (ii/annot--)))))

(defun ii/annot-follow-links (pos)
  "Prompt to follow one of the links in annotation at POS."
  (interactive (list (point)))
  (message "Not implemented yet!"))

(provide 'implicit-annotations)
;;; implicit-annotations.el ends here
