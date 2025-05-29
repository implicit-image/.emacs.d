;;; -*- lexical-binding: t -*-

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
         (corfu-terminal-mode (if (display-graphic-p) -1 1))))

(add-hook 'tty-setup-hook '+tty-setup)

(use-package kkp
  :hook
  (tty-setup-hook . (lambda ()
                      (interactive)
                      (global-kkp-mode +1))))

(use-package evil-terminal-cursor-changer
  :commands
  (etcc-on)
  :init
  (setq evil-motion-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-emacs-state-cursor  'hbar)
  :hook
  (tty-setup-hook . etcc-on))

(provide 'init-tty)
