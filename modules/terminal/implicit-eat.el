;;; -*- lexical-binding: t -*-

(defun +eat--setup ()
  "Setup `eat-mode'.")


(defun +eat/compile (command arg)
  (interactive (list
                (read-shell-command "compile command: "
                                    compile-command
                                    'compile-history)
                current-prefix-arg))
  (let* ((default-directory (or (project-root (project-current))
                                default-directory)))
    (pop-to-buffer (eat command t))))

;; (defun +eat/run-tui-program (cmd)
;;   (interactive (list (read-shell-command "program: "
;;                                          nil
;;                                          'shell-command-history)))
;;   (let ((default-directory (or (project-root (project-current))
;;                                default-directory)))
;;     (with-current-buffer (eat-make))



(provide 'implicit-eat)
