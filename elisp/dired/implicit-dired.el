;;; implicit-dired.el --- Dired and file utilities -*- lexical-binding: t -*-

;; Author: Błażej Niewiadomski
;; Version: 0.1
;; Package-Requires: dependencies
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
(require 'dired)

(defgroup ii/fd-dired nil
  "Run a `fd' command and Dired the output."
  :group 'dired
  :prefix "ii/fd-")

;; FIXME this option does not really belong in this file, it's more general.
;; Eg cf some tests in grep.el.
(defcustom ii/fd-exec-terminator
  (if (eq 0
          (ignore-errors
            (process-file find-program nil nil nil
                          null-device "-exec" "echo" "{}" "+")))
      "+"
    (shell-quote-argument ";"))
  "String that terminates \"find -exec COMMAND {} \".
The value should include any needed quoting for the shell.
Common values are \"+\" and \"\\\\;\", with the former more efficient
than the latter."
  :version "24.1"
  :group 'ii/fd-dired
  :type 'string)

;; (defvar find-ls-option-default-ls
;;   (cons "-ls" (if find-gnu-find-p "-dilsb" "-dgils")))
;;
;; (defvar find-ls-option-default-exec
;;   (cons (format "-exec ls -ld {} %s" ii/fd-exec-terminator) "-ld"))
;;
;; (defvar find-ls-option-default-xargs
;;   (cons "-print0 | sort -z | xargs -0 -e ls -ld" "-ld"))

;; find's -ls corresponds to these switches.
;; Note -b, at least GNU find quotes spaces etc. in filenames
(defcustom ii/fd-ls-option (cons "-l" "-lh")
  ;; (if (eq 0
  ;;         (ignore-errors
  ;;           (process-file find-program nil nil nil null-device "-ls")))
  ;;     find-ls-option-default-ls
  ;;   find-ls-option-default-exec)

  "A pair of options to produce and parse an `ls -l'-type list from `find'.
This is a cons of two strings (FIND-OPTION . LS-SWITCHES).
FIND-OPTION is the option (or options) passed to `find' to produce
a file listing in the desired format.  LS-SWITCHES is a set of
`ls' switches that tell Dired how to parse the output of `find'.

The two options must be set to compatible values.
For example, to use human-readable file sizes with GNU ls:
   (\"-exec ls -ldh {} +\" . \"-ldh\")

To use GNU find's inbuilt \"-ls\" option to list files:
   (\"-ls\" . \"-dilsb\")
since GNU find's output has the same format as using GNU ls with
the options \"-dilsb\".

While the option `find -ls' often produces unsorted output, the option
`find -exec ls -ld' maintains the sorting order only on short output,
whereas `find -print | sort | xargs' produces sorted output even
on a large number of files.")

(defcustom ii/fd-ls-subdir-switches "-al"
  "`ls' switches for inserting subdirectories in `*Find*' buffers.
This should contain the \"-l\" switch.
Use the \"-F\" or \"-b\" switches if and only if you also use
them for `find-ls-option'.")

(defcustom ii/fd-rg-options "-q "

  "Option to grep to be as silent as possible.
On Berkeley systems, this is `-s'; on Posix, and with GNU grep, `-q' does it.
On other systems, the closest you can come is to use `-l'.")

;; This used to be autoloaded (see bug#4387).
(defcustom ii/fd-name-arg
  (if read-file-name-completion-ignore-case
      "-i"
    "-s")
  "Argument used to specify file name pattern.
If `read-file-name-completion-ignore-case' is non-nil, -iname is used so that
find also ignores case.  Otherwise, -name is used.")

(defcustom ii/fd-dired-refine-function #'ignore
  "If non-nil, a function for refining the *Find* buffer of `ii/fd-dired'.
This function takes no arguments.  The *Find* buffer is narrowed to the
output of `find' (one file per line) when this function is called.")

(defvar ii/fd-rg-dired-suffix "xargs -0 ls --quoting-style=literal  %s ")

(defvar ii/fd-dired-args " --color=never --type f "
  "test")

(defvar ii/fd-dired-program (executable-find "fd"))

;; History of find-args values entered in the minibuffer.
(defvar ii/fd-args-history nil)

(defvar ii/fd-command-history nil
  "History of commands passed interactively to `ii/fd-dired-with-command'.")

;; for lexical binding
(defvar dired-sort-inhibit)

(defun ii/fd-dired--excaped-ls-option ()
  "Return the car of `ii/fd-ls-option' escaped for a shell command."
  (if (string-match "\\`\\(.*\\) {} \\(\\\\;\\|\\+\\)\\'"
                    (car ii/fd-ls-option))
      (format "%s %s %s"
              (match-string 1 (car ii/fd-ls-option))
              (shell-quote-argument "{}")
              ii/fd-exec-terminator)
    (car ii/fd-ls-option)))

(defun ii/fd-dired-filter (proc string)
  ;; Filter for \\[ii/fd-dired] processes.
  (let ((buf (process-buffer proc))
        (inhibit-read-only t))
    (if (and (buffer-name buf) (not (string-match-p "^//DIRED.*" string)))
        (with-current-buffer buf
          (save-excursion
            (save-restriction
              (widen)
              (let ((buffer-read-only nil)
                    (beg (point-max)))
                (goto-char beg)
                (insert string)
                (goto-char beg)
                (or (looking-at "^")
                    (forward-line 1))
                (while (looking-at "^")
                  (insert "  ")
                  (forward-line 1))
                ;; Convert ` ./FILE' to ` FILE'
                ;; This would lose if the current chunk of output
                ;; starts or ends within the ` ./', so back up a bit:
                (goto-char (- beg 3))	; no error if < 0
                (while (search-forward " ./" nil t)
                  (delete-region (point) (- (point) 2)))
                ;; Find all the complete lines in the unprocessed
                ;; output and process it to add text properties.
                (goto-char (point-max))
                (if (search-backward "\n" (process-mark proc) t)
                    (progn
                      (dired-insert-set-properties (process-mark proc)
                                                   (1+ (point)))
                      (move-marker (process-mark proc) (1+ (point)))))))))
      ;; The buffer has been killed.
      (delete-process proc))))

(defun ii/fd-dired-sentinel (proc state)
  "Sentinel for \\[ii/fd-dired] processes."
  (let ((buf (process-buffer proc)))
    (if (buffer-name buf)
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (save-excursion
              (save-restriction
                (widen)
                (when ii/fd-dired-refine-function
                  ;; `ii/fd-dired-filter' puts two whitespace characters
                  ;; at the beginning of every line.
                  (narrow-to-region (point) (- (point-max) 2))
                  (funcall ii/fd-dired-refine-function)
                  (widen))
                (let ((point (point-max)))
                  (goto-char point)
                  (insert "\n  find "
                          (substring state 0 -1) ; omit \n at end of STATE.
                          " at " (substring (current-time-string) 0 19))
                  (dired-insert-set-properties point (point))))
              (setq mode-line-process
                    (format ":%s" (process-status proc)))
              ;; Since the buffer and mode line will show that the
              ;; process is dead, we can delete it now.  Otherwise it
              ;; will stay around until M-x `list-processes'.
              (delete-process proc)
              (force-mode-line-update))))
      (message "ii/fd-dired %s finished." buf))))

(defun ii/fd-kill-process ()
  "Kill the `fd' process running in the current buffer."
  (interactive)
  (let ((fd (get-buffer-process (current-buffer))))
    (and fd (eq (process-status fd) 'run)
         (eq (process-filter fd) #'ii/fd-dired-filter)
         (condition-case nil
             (delete-process fd)
           (error nil)))))

(defun ii/fd-dired-sort-by-filename ()
  "Sort entries in *Fd* buffer by file name lexicographically."
  (sort-subr nil 'forward-line 'end-of-line
             (lambda ()
               (when-let* ((start
                            (next-single-property-change
                             (point) 'dired-filename)))
                 (buffer-substring-no-properties start (line-end-position))))))

;;;###autoload
(defun ii/fd-dired (dir args &optional suffix)
  "Run `fd' and got into `dired-mode' on a buffer of the output"
  (interactive (list (read-directory-name "Run fd in directory: " nil "" t)
                     (read-string "Run fd (with args): " ii/fd-dired-args
                                  (if ii/fd-dired-args
                                      '(ii/fd-args-history . 1)
                                    'ii/fd-args-history))))
  (setq ii/fd-dired-args args
        args (concat ii/fd-dired-program
                     " "
                     (ii/fd-dired--excaped-ls-option)
                     " "
                     (if (string= args "")
                         ""
                       (concat (shell-quote-argument "(")
                               " " args " "
                               (shell-quote-argument ")")
                               " "))
                     "."
                     (or (and suffix (concat " " suffix)) "")))
  (ii/fd-dired-with-command dir args))

;; TODO: tidy up the command
;;;###autoload
(defun ii/fd-rg-dired (dir file-name-regexp content-regexp)
  "Find files in DIR that contain matches for REGEXP and start Dired on output.
The command run after changing into DIR is

fd -exec `ii/rg-program' `ii/fd-rg-options' -e REGEXP {}; -ls DIR."
  (interactive (list (read-directory-name "Fd-Rg (directory): ")
                     (read-regexp "Fd-Rg (file name regexp): ")
                     (read-regexp "Fd-Rg (grep regexp): ")))
  (ii/fd-dired-with-command dir
                            (concat
                             ii/fd-dired-program
                             ii/fd-dired-args
                             " -X rg -0 --files-with-matches -e "
                             (shell-quote-argument content-regexp)
                             " "
                             (shell-quote-argument "{}")
                             " "
                             (shell-quote-argument ";")
                             " "
                             (shell-quote-argument file-name-regexp)
                             " . "
                             " | " (format ii/fd-rg-dired-suffix "-lh" "%F"))))

;;;###autoload
(defun ii/fd-rg-dired-glob (dir glob-pattern content-regexp)
  "Find files in DIR that contain matches for REGEXP and start Dired on output.
The command run after changing into DIR is

fd -exec `ii/rg-program' `ii/fd-rg-options' -e REGEXP {}; -ls DIR."
  (interactive (list (read-directory-name "Fd-Rg (directory): ")
                     (read-regexp "Fd-Rg (shell glob): ")
                     (read-regexp "Fd-Rg (grep regexp): ")))
  (ii/fd-dired-with-command dir
                            (concat
                             ii/fd-dired-program
                             ii/fd-dired-args
                             " -X rg -0 --files-with-matches -e "
                             (shell-quote-argument content-regexp)
                             " "
                             (shell-quote-argument "{}")
                             " "
                             (shell-quote-argument ";")
                             " --glob " (shell-quote-argument glob-pattern)
                             " . "
                             " | " (format ii/fd-rg-dired-suffix "-lh" "%F"))))

;;;###autoload
(defun ii/fd-dired-glob (dir glob-pattern)
  (interactive (list (read-directory-name "Fd (directory): ")
                     (read-string "Fd (shell glob): ")))
  (ii/fd-dired-with-command dir
                            (concat
                             ii/fd-dired-program
                             ii/fd-dired-args
                             " --glob " (shell-quote-argument glob-pattern))
                            " . "))

;;;###autoload
(defun ii/fd-dired-with-command (dir command)
  "Run `fd' and go into Dired mode on a buffer of the output.
The user-supplied COMMAND is run after changing into DIR and should look like

    fd . GLOBALARGS \\( ARGS \\) -ls

The car of the variable `find-ls-option' specifies what to
use in place of \"-ls\" as the starting input.

Collect output in the \"*Fd*\" buffer.  To kill the job before
it finishes, type \\[kill-find]."
  (interactive
   (list (read-directory-name "Run fd in directory: " nil "" t)
         (read-string "Run fd command: "
                      (cons (concat ii/fd-dired-program
                                    " . \\(  \\) "
                                    (ii/fd-dired--excaped-ls-option))
                            (+ 1 (length ii/fd-dired-program) (length " . \\( ")))
                      'ii/fd-dired-history)))
  (let ((dired-buffers dired-buffers))
    ;; Expand DIR ("" means default-directory), and make sure it has a
    ;; trailing slash.
    (setq dir (file-name-as-directory (expand-file-name dir)))
    ;; Check that it's really a directory.
    (or (file-directory-p dir)
        (error "fd-dired needs a directory: %s" dir))
    (pop-to-buffer-same-window (get-buffer-create (format "*Fd Dired: %s*" dir)))

    ;; See if there's still a `find' running, and offer to kill
    ;; it first, if it is.
    (let ((fd (get-buffer-process (current-buffer))))
      (when fd
        (if (or (not (eq (process-status fd) 'run))
                (yes-or-no-p
                 (format-message "A `fd' process is running; kill it? ")))
            (condition-case nil
                (progn
                  (interrupt-process fd)
                  (sit-for 1)
                  (delete-process fd))
              (error nil))
          (error "Cannot have two processes in `%s' at once" (buffer-name)))))

    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq default-directory dir)
    ;; Start the find process.
    (let ((proc (start-file-process-shell-command
                 (buffer-name) (current-buffer) command)))
      ;; Initialize the process marker; it is used by the filter.
      (move-marker (process-mark proc) (point) (current-buffer))
      (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)
      (set-process-filter proc #'ii/fd-dired-filter)
      (set-process-sentinel proc #'ii/fd-dired-sentinel))
    (dired-mode dir (cdr ii/fd-ls-option))
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (define-key map "\C-c\C-k" 'kill-find)
      (use-local-map map))
    (setq-local dired-sort-inhibit t)
    (setq-local revert-buffer-function
                (lambda (_ignore-auto _noconfirm)
                  (ii/fd-dired-with-command dir command)))
    ;; Set subdir-alist so that Tree Dired will work:
    (if (fboundp 'dired-simple-subdir-alist)
        ;; will work even with nested dired format (dired-nstd.el,v 1.15
        ;; and later)
        (dired-simple-subdir-alist)
      ;; else we have an ancient tree dired (or classic dired, where
      ;; this does no harm)
      (setq dired-subdir-alist
            (list (cons default-directory (point-min-marker)))))
    (setq-local dired-subdir-switches ii/fd-ls-subdir-switches)
    (setq buffer-read-only nil)
    ;; Subdir headlerline must come first because the first marker in
    ;; subdir-alist points there.
    (insert "  " dir ":\n")
    (when dired-make-directory-clickable
      (dired--make-directory-clickable))
    ;; Make second line a ``find'' line in analogy to the ``total'' or
    ;; ``wildcard'' line.
    ;; (let ((point (point)))
    ;;   (insert "  " command "\n")
    ;;   (dired-insert-set-properties point (point)))
    (setq buffer-read-only t)
    (setq mode-line-process '(":%s"))))

;;;###autoload
(defun ii/fd-rg-dired-project (file-name-regexp content-regexp)
  (interactive (list (read-regexp "Fd-Rg (file regexp): ")
                     (read-regexp "Fd-Rg (grep regexp): ")))
  (ii/with-project-root! dir
    (ii/fd-rg-dired dir file-name-regexp content-regexp)))

;;;###autoload
(defun ii/fd-rg-dired-glob-project (glob-pattern content-regexp)
  (interactive (list (read-regexp "Fd-Rg (shell glob): ")
                     (read-regexp "Fd-Rg (grep regexp): ")))
  (ii/with-project-root! dir
    (ii/fd-rg-dired-glob dir glob-pattern content-regexp)))

;;;###autoload
(defun ii/fd-dired-project (args)
  (interactive (list (read-string "Run fd (with args): " ii/fd-dired-args
                                  (if ii/fd-dired-args
                                      '(ii/fd-args-history . 1)
                                    'ii/fd-args-history))))
  (ii/with-project-root! dir
    (ii/fd-dired dir args)))

;;;###autoload
(defun ii/fd-dired-glob-project (glob-pattern)
  (interactive (list (read-string "Fd (shell glob): ")))
  (ii/with-project-root! root
    (ii/fd-dired-glob root glob-pattern)))

(provide 'implicit-dired)
;;; implicit-dired.el ends here
