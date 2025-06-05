
;;;###autoload
(defun +tty-setup ()
  "Run some code to adjust emacs for tty experience."
  ;;;; set up faces for tty
  (progn (custom-set-faces `(vertical-border ((t (:background ,(doom-color 'bg) :foreground ,(doom-color 'fg-alt) :width narrow))))
                           `(border ((t (:background ,(doom-color  'bg) :foreground ,(doom-color 'fg) :width narrow))))
                           `(internal-border ((t (:background ,(doom-color 'bg) :width narrow))))
                           `(whitespace-space ((t (:foreground ,(doom-color 'bg))))))
         (set-display-table-slot standard-display-table
                                 'vertical-border
                                 (make-glyph-code ?┃))
         (set-display-table-slot standard-display-table 'truncation ?\u2192)
         (setq visible-cursor nil)
;;;; corfu-terminal
         (when (and (not (bound-and-true-p corfu-terminal-mode))
                    (display-graphic-p))
           (corfu-terminal-mode 1))))

(provide 'init-tty-impl)
