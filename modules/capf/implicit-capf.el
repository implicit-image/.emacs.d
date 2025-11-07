(require 'corfu)
(require 'cape)
;;(require 'corfu-terminal)

(defun +cape--setup ()
  (add-hook 'completion-at-point-functions 'cape-file))

;;;###autoload
(defun +global-corfu-mode--setup ()
  (+cape--setup)
  (corfu-echo-mode 1)
  (corfu-history-mode 1))
;;  (if (not (display-graphic-p))
;;      (corfu-terminal-mode 1)
;;    (corfu-popupinfo-mode 1)))

;;;###autoload
(defun +corfu-toggle-auto ()
  (interactive)
  (when (bound-and-true-p corfu-mode)
    (+toggle-var! corfu-auto)
    (+toggle-var! corfu-preview-current t 'insert)
    (corfu-mode 1)))

(defun +completion--in-region (start end collection pred)
  "START END COLLECTION PRED."
  (cond ((display-graphic-p) (corfu--in-region start end collection pred))
        (t (consult-completion-in-region start end collection pred))))


(provide 'implicit-capf)
