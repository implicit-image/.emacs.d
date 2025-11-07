;;; -*- lexical-binding: t -*-

(defun +tty-display-update ()
  "Run some code to adjust emacs for tty experience."
  (message "Updating display settings")
  (cond ((display-graphic-p)
         (custom-set-faces `(whitespace-space ((t (:foreground ,(doom-lighten (doom-color 'bg-alt) 0.1))))))
         (when corfu-terminal-mode
           (corfu-terminal-mode -1)))
        (t (custom-set-faces `(whitespace-space ((t (:foreground ,(doom-color 'bg))))) )
           (corfu-terminal-mode 1))))


(provide 'implicit-tty)
