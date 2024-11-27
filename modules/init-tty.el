
(setq visible-cursor nil)

;;;###autoload
(defun +tty-setup-faces ()
  "Setup faces for tty display."


  (progn (custom-set-faces `(vertical-border ((t (:background ,(doom-color 'bg) :foreground ,(doom-color 'fg-alt) :width narrow))))
			   `(border ((t (:background ,(doom-color  'bg) :foreground ,(doom-color 'fg) :width narrow))))
			   `(internal-border ((t (:background ,(doom-color 'bg) :width narrow)))))
	 (set-display-table-slot standard-display-table
				 'vertical-border
				 (make-glyph-code ?┃))))

(add-hook 'tty-setup-hook #'+tty-setup-faces)

(use-package kkp
  :demand
  :config
  (global-kkp-mode +1))

(use-package evil-terminal-cursor-changer
  :demand
  :init
  (setq evil-motion-state-cursor 'box
	evil-visual-state-cursor 'box
	evil-normal-state-cursor 'box
	evil-insert-state-cursor 'bar
	evil-emacs-state-cursor  'hbar)
  :hook
  (tty-setup . etcc-on))

(provide 'init-tty)
