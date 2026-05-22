;;; implicit-async.el --- Async emacs processing -*- lexical-binding: t -*-

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

(require 'server)

(declare-function async-inject-variables "async")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ii/async--server-buffer-prefix "async-srv")

(defvar ii/async--connection-buffer-prefix "async-con")

(defvar ii/async-server-pool (make-hash-table :test 'eq :size 5))

(defvar ii/async-session-server-table (make-hash-table :test 'eq :size 5)
  "Hash table matching session IDs to server names.")

(defvar-local ii/async-server-init-callback nil
  "Function to execute after the server finishes the initialization.")

(defvar-local ii/async-server-name nil)

(defvar-local ii/async-connection-callback nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii/async--mark-server-free (server)
  "Set state of SERVER to `free'."
  (puthash server 'free ii/async-server-pool))

(defun ii/async--mark-server-busy (server)
  "Set state of SERVER to `busy'."
  (puthash server 'busy ii/async-server-pool))

(defun ii/async--mark-server-initializing (server)
  "Set state of SERVER to `init'."
  (puthash server 'init ii/async-server-pool))

(defun ii/async--server-state-p (server state)
  "Check if state of SERVER is STATE."
  (when (and (stringp server) (memq state '(init busy free)))
    (eq (gethash server ii/async-server-pool) state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conncection manageement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii/async--connection-sentinel (proc change)
  "Process sentinel for connection process PROC. If CHANGE ."
  (message "Server connection finished with status: %S in buffer %S"
           (process-status proc)
           (process-buffer proc))
  (when (memq (process-status proc) '(closed exit failed))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-min))
      ;; If the result is nil, there's nothing in the buffer.  If the
      ;; result is non-nil, it's after "-print ".
      (let ((answer "")
            (callback (buffer-local-value 'ii/async-connection-callback (current-buffer))))
        (while (re-search-forward "\n-print\\(-nonl\\)? " nil t)
          (setq answer
                (concat answer
                        (buffer-substring (point)
                                          (progn (skip-chars-forward "^\n")
                                                 (point))))))
        ;; clear out the buffer to avoid accumulating megabytes of text
        ;; (erase-buffer)
        (message "Connection answer is %s" answer)
        (if (not (equal answer ""))
            (condition-case err
                (let ((res (read
                            (decode-coding-string (server-unquote-arg answer)
                                                  'emacs-internal))))
                  (when callback (funcall callback res))
                  ;; (when (and res-place (symbolp res-place))
                  ;;   (set res-place res))
                  res)
              ;; Re-signal with a more specific condition.
              (invalid-read-syntax
               (signal 'server-return-invalid-read-syntax
                       (cdr err))))
          (when callback (funcall callback nil))))
      (ii/async--mark-server-free (buffer-local-value 'ii/async-server-name (current-buffer)))
      ;; (if (and (buffer-local-boundp 'ii/async-server-name (current-buffer))
      ;;          (stringp (buffer-local-value 'ii/async-server-name (current-buffer))))
      ;;     (ii/async--mark-server-free (buffer-local-value 'ii/async-server-name (current-buffer)))
      ;;   (message "Connection server name for %s not found" (buffer-name)))

      ;; (kill-buffer (process-buffer proc))
      )))

(defun ii/async--get-connection-buffer (server &optional create)
  "Get the buffer for connection process for SERVER. If CREATE is non-nil, \
create a new buffer if it doesnt exist."
  (funcall (if create #'get-buffer-create #'get-buffer)
           (format " *%s-%s*" ii/async--connection-buffer-prefix server)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; server process management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii/async--get-server-buffer (server-name &optional create)
  "Get the buffer for server named SERVER-NAME. If CREATE is non-nil, \
create a new buffer if it doesnt exist."
  (funcall (if create #'get-buffer-create #'get-buffer)
           (format " *%s-%s*" ii/async--server-buffer-prefix server-name)))

(defun ii/async--get-new-server-name ()
  "Get a unique name for a new server."
  (let* ((len (1+ (hash-table-count ii/async-server-pool)))
         (name (format "ii-async-%d" (+ len (random (* len 100))))))
    (while (gethash name ii/async-server-pool)
      (setq name (format "ii-async-%d" (+ len (random (* len 100))))))
    name))

(defun ii/async--get-free-server ()
  "Get the name of the first nonbusy server in `ii/async-server-pool'."
  (catch 'free
    (dolist (name (hash-table-keys ii/async-server-pool))
      (if (ii/async--server-state-p name 'free)
          (throw 'free name)
        (message "server %s not free: %S" name (gethash name ii/async-server-pool))))))

(defun ii/async--server-sentinel (proc status)
  "Process sentinel for the server process PROC. If STATUS indicates that the process \
is finished kill the process buffer and update server pool."
  (message "server status: %S\b message: %s" (process-status proc) status)
  (when (memq (process-status proc) '(exit closed))
    (when-let* ((buf (process-buffer proc)))
      (with-current-buffer buf
        (when (and (buffer-local-boundp 'ii/async-server-init-callback buf)
                   (functionp (buffer-local-value 'ii/async-server-init-callback buf)))
          (funcall ii/async-server-init-callback ii/async-server-name)
          (setq-local ii/async-server-init-callback nil))
        (ii/async--mark-server-free ii/async-server-name)))))

(defun ii/async--start-new-server (callback &optional name)
  "Start server NAME and add it to the server pool."
  (let* ((name (or name (ii/async--get-new-server-name)))
         (buf (ii/async--get-server-buffer name t))
         (proc (start-process name buf "emacs" "-q" (format "--bg-daemon=%s" name))))
    (set-process-sentinel proc #'ii/async--server-sentinel)
    (message "starting new server %s" name)
    (ii/async--mark-server-initializing name)
    (with-current-buffer buf
      (setq-local ii/async-server-init-callback callback
                  ii/async-server-name name))
    name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii/async--server-eval-at (server form &optional callback res-place)
  "Eval FORM at Emacs server SERVER. If CALLBACK is a function , it is called with the return value of \
FORM as the only argument. If RES-PLACE is a symbol, set its value to the return value of FORM."
  (let* ((server-dir (if server-use-tcp server-auth-dir server-socket-dir))
         (server-file (expand-file-name server server-dir))
         ;; decide what to do without callback provided
         ;; (callback (or callback #'ii/async-server-default-callback))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (buf (ii/async--get-connection-buffer server t))
         address port secret process)
    ;; make sure the server is running
    (unless (file-exists-p server-file)
      (error "No such server: %s" server))

    (with-current-buffer buf
      (message "setting up connection")
      (erase-buffer)
      (when server-use-tcp
        (let ((coding-system-for-read 'no-conversion))
          (insert-file-contents server-file)
          (unless (looking-at "\\([0-9.]+\\):\\([0-9]+\\)")
            (error "Invalid auth file"))
          (setq address (match-string 1)
                port (string-to-number (match-string 2)))
          (forward-line 1)
          (setq secret (buffer-substring (point) (line-end-position)))
          (erase-buffer)))
      (unless (setq process (make-network-process
                             :name "eval-at"
                             :buffer buf
                             :host address
                             :service (if server-use-tcp port server-file)
                             :family (if server-use-tcp 'ipv4 'local)
                             :noquery t))
        (error "Unable to contact the server"))

      (ii/async--mark-server-busy server)
      (setq-local ii/async-server-name server
                  ii/async-connection-callback callback)
      (set-process-sentinel process #'ii/async--connection-sentinel)

      ;; send form for evaluation
      (when server-use-tcp
        (process-send-string process (concat "-auth " secret "\n")))
      (message "Sending form %S" form)
      (process-send-string process
                           (concat "-eval "
                                   (server-quote-arg (format "%S" form))
                                   " \n"))
      server)))

;;TODO: implement forced kill
(defun ii/async--kill-server (server &optional force)
  "Send a command to kill SERVER. If FORCE is non-nil, ignore server state."
  (cond
   ((or (ii/async--server-state-p server 'free) force)
    (ignore-errors (ii/async--server-eval-at server '(kill-emacs)
                                             (lambda (res)
                                               (remhash server ii/async-server-pool)))))))

(defun ii/async--kill-all (&optional force)
  "Kill all server processes and clear the server pool."
  (mapc (lambda (server)
          (ignore-errors
            (ii/async--kill-server server force)))
        (hash-table-keys ii/async-server-pool)))

(defun ii/async--eval (form &optional callback)
  "Evaluate FORM on an avaiable Emacs server. If CALLBACK is non-nil,\
it should be a function called with 1 argumet, the return value of FORM."
  (let ((free (ii/async--get-free-server)))
    (cond
     (free
      (message "found free server: %s" free)
      (ii/async--server-eval-at free form callback))
     (t
      (message "free server not found, creating new one")
      (ii/async--start-new-server (lambda (server-name)
                                    (ii/async--server-eval-at server-name form callback)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parallel execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ii/async--parallelized-number-of-cores (pcase system-type
                                                 ('gnu/linux (length (string-lines (shell-command-to-string "cat /proc/cpuinfo | grep \"processor\""))))
                                                 (_ 4)))

(defvar ii/async-parallelized-results-table (make-hash-table :test 'eq :size 10))

(defvar ii/async-parallelized-callback-table (make-hash-table :test 'eq :size 10))

(defun ii/async--get-parallelized-op-id (seq num)
  (let ((object-hash (sxhash (list num seq (float-time)))))
    object-hash))

(defun ii/async--store-parallelized-result (value id idx)
  (message "Storing result %S for id %d" value id)
  (let ((res (gethash id ii/async-parallelized-results-table)))
    (when (< idx (length res))
      (aset res idx value))))

(defun ii/async--setup-parallelized-result (num-of-threads id)
  (message "Setting up result for %S" id)
  (puthash id (make-vector num-of-threads nil) ii/async-parallelized-results-table))

(defun ii/async--parallelized-result-valid-p (op-id)
  (message "Validating result for op: %d" op-id)
  (when-let* ((res (gethash op-id ii/async-parallelized-results-table)))
    (when (length> res 0)
      (let ((first-type (type-of (aref res 0))))
        (and (seq-every-p (lambda (el)
                            (eq first-type (type-of el)))
                          res))))))

(defun ii/async--parallelized-concat-result (res)
  (message "Concating res-vector %S" res-vector)
  (when (and (length> res-vector 1))
    (when-let* ((first-type (type-of (aref res-vector 0)))
                (type (pcase first-type
                        ('cons 'list)
                        ((or 'vector 'string 'list) first-type)
                        (_ nil)))
                (results (let ((acc (list))
                               (len (length res-vector))
                               (n 0))
                           (while (< n len)
                             (push (aref res-vector n) acc)
                             (incf n))
                           (reverse acc))))
      (message "concatting %S" results)
      (apply #'seq-concatenate type results))))

(defun ii/async--setup-parallelized-callback (id callback)
  (puthash id callback ii/async-parallelized-callback-table))

(defun ii/async--parallelized-op-complete-p (op-id)
  (and (not (seq-some #'null (gethash op-id ii/async-parallelized-results-table)))
       (ii/async--parallelized-result-valid-p op-id)))

(defun ii/async--parallelized-get-callback (op-id)
  (gethash op-id ii/async-parallelized-callback-table))

(defun ii/async--parallelized-callback (res)
  "Execute body when ."
  (message "PARALELL_CALLBACK CALLED WITH %S" res)
  (when (and (listp res) (length> res 4))
    (let ((idx (nth 0 res))
          (op-id (nth 1 res))
          (size (nth 2 res))
          (val (nth 3 res)))
      (when (null (gethash op-id ii/async-parallelized-results-table))
        (ii/async--setup-parallelized-result size op-id))
      (ii/async--store-parallelized-result val op-id idx)

      ;; check if the operaion collected all results
      (let* ((callback (ii/async--parallelized-get-callback op-id))
             (completed (ii/async--parallelized-op-complete-p op-id)))
        (cond
         (completed
          (message "op %d finished" op-id)
          (when (functionp callback)
            (message "Calling final callback")
            (funcall callback (ii/async--parallelized-concat-result (gethash op-id ii/async-parallelized-results-table)))
            (remhash op-id ii/async-parallelized-callback-table)))
         (t
          (message "received partial result %s for op %s" val op-id)))))))


(defun ii/async-parallelize (sequence form &optional callback num)
  (let* ((num (min (or num most-positive-fixnum) ii/async--parallelized-number-of-cores))
         (split (seq-split sequence (/ (length sequence) (1- num))))
         (id (ii/async--get-parallelized-op-id sequence num))
         (size (length split)))
    ;; register callback for this op
    (when callback (puthash id callback ii/async-parallelized-callback-table))
    (message "PARALLELIZE: START")
    (message "split list is %S" split)
    ;; start all proceses
    (dotimes (i size)
      (ii/async--eval
       `(let ((sequence ',(nth i split))
              index result id size)
          (setq index ,i
                size ,size
                id ,id
                result (progn ,form))
          (list index id size result))
       #'ii/async--parallelized-callback))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; state injection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ii/async-inject-let-bindings (include-regexp &optional predicate exclude-regexp)
  "Return a list of bindings of variables matching INCLUDE-REGEXP for `let' form.If PREDICATE is a function, it is used to filter matched symbols. If EXCLUDE-REGEXP is non-nil,
dont include any symbols matching it."
  `(,@(let (bindings)
        (mapatoms
         (lambda (sym)
           (let ((sname (and (boundp sym) (symbol-name sym)))
                 value)
             (when (and sname
                        (or (null include-regexp)
                            (string-match include-regexp sname))
                        (or (null exclude-regexp)
                            (not (string-match exclude-regexp sname)))
                        (cl-loop for re in async-inject-variables-exclude-regexps
                                 never (string-match-p re sname)))
               (setq value (symbol-value sym))
               (unless (or (stringp value)
                           (memq value '(nil t))
                           (numberp value)
                           (vectorp value))
                 (setq value `(quote ,value)))
               ;; (when noprops
               ;;   (setq value (funcall async-variables-noprops-function
               ;;                        value)))
               (when (or (null predicate)
                         (funcall predicate sym))
                 (push (list sym value) bindings))))))
        bindings)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; automatic cleanup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ii/async-cleanup-timer nil)


;; (defun ii/async--cleanup-buffers ()
;;   (let* ((bufs (match-buffers (format " \*\\(%s\\|%s\\)-\*" ii/async--server-buffer-prefix
;;                                       ii/async--connection-buffer-prefix)))
;;          (one-free nil))
;;     (dolist (buf match)
;;       (let ((server (buffer-local-value 'ii/async-server-name buf)))
;;         (when (ii/async--server-state-p server 'free)
;;           (if (null one-free)
;;               (setq one-free t)
;;             (ii/async--kill-server server)
;;             (kill-buffer buf)))))))
;;
;; (defun ii/async--auto-cleanup ()
;;   (ii/async--cleanup-buffers)
;;   (when ii/async-server-pool
;;     ))
;;
;; (define-minor-mode ii/async-auto-cleanup-mode
;;   "Global minor mode for automaticaaly killing hanging async servers."
;;   (if ii/async-auto-cleanup-mode
;;       (setq ii/async-cleanup-timer (run-with-idle-timer 1.0 t #'ii/async--auto-cleanup))
;;     (when (timerp ii/async-cleanup-timer)
;;       (cancel-timer ii/async-cleanup-timer))))

(provide 'implicit-async)
;;; implicit-async.el ends here

