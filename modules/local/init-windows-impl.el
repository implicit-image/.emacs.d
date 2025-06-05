


(defun +windows/split-vertically-prefix ()
  (interactive)
  (display-buffer-override-next-command
   (lambda (buffer alist)
     (let ((alist (append '((inhibit-same-window . t)) alist))
           window
           type)
       (let (setq window (display-buffer-)))))))



(provide 'init-windows-impl)
