;;;###autoload
(defun +global-corfu-mode--setup ()
  (corfu-echo-mode 1)
  (corfu-history-mode 1)
  (if (not (display-graphic-p))
      (corfu-terminal-mode 1)
    (corfu-popupinfo-mode 1)))

;;;###autoload
(defun +corfu-toggle-auto ()
  (interactive)
  (when (bound-and-true-p corfu-mode)
    (+toggle-var! corfu-auto)
    (corfu-mode 1)))

(provide 'init-corfu-impl)
