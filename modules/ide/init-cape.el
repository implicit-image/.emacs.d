;;; -*- lexical-binding: t -*-

(use-package cape
  :init

  (defvar +cape-setup-hook nil
    "Hook to run after initializing corfu completion in buffer.")

  (defun +yasnippet-capf ()
    (yasnippet-capf nil))

  (defun +cape-setup ()
    "Setup completion backends."
    ;; save existing completions
    (interactive)
    (let ((old-capf completion-at-point-functions))
      (add-hook 'completion-at-point-functions 'cape-file)
      (cond ((memq 'elisp-completion-at-point completion-at-point-functions)
             (progn (remove-hook 'completion-at-point-functions 'elisp-completion-at-point)
                    (add-hook 'completion-at-point-functions (cape-capf-super 'elisp-completion-at-point
                                                                              '+yasnippet-capf))))
            (t nil)))))

(provide 'init-cape)
