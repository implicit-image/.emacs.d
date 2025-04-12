;;; -*- lexical-binding: t -*-


(use-package frame
  :straight nil
  :config
  (setq window-divider-default-places 'right-only
        window-divider-default-right-width 1
        window-divider-default-bottom-width 1)
  (window-divider-mode))

(use-package ace-window
  :after popwin
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :general
  (+leader-keys
    "w w" '("Switch" . ace-window))
  (global-map
   "<remap> <evil-window-next>" 'ace-window
   "C-x w" 'ace-window))

(use-package window
  :straight nil)

(use-package popwin
  :init
  (defun +windows-cfg (&rest popwin-cfg-forms)
    "Each one of POPWIN-CFG-FORMS is (BUFFER-NAMES . POPWIN-OPTIONS-PLIST)."
    (require 'popwin)
    (mapc (lambda (cfg-form)
            (let ((buffers (car cfg-form))
                  (cfg-opts (cdr cfg-form)))
              (mapc (lambda (buffer)
                      (add-to-list 'popwin:special-display-config
                                   (append (list buffer) cfg-opts)))
                    buffers)))
          popwin-cfg-forms))

  :config
  (+windows-cfg
   '(("\*Warnings\*" "\*Warnings\**" "\*scratch\*" shell-mode help-mode)
     :regexp t :height 0.3 :position bottom :dedicated nil :noselect t))
  :general
  (+leader-keys
    "b p" '("Popup buffer" . popwin:popup-buffer)
    "~" '("Show last popup" . popwin:popup-last-buffer))
  :hook
  (after-init . (lambda ()
                  (interactive)
                  (popwin-mode 1))))

(use-package popper
  :init
  (defun +popper-setup ()
    "Set up popper"
    (popper-mode 1)
    (popper-echo-mode 1))

  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          help-mode
          compilation-mode))
  :hook
  (after-init . +popper-setup)
  :general
  (+leader-keys
    "b TAB" '("Popup buffers" . popper-cycle)
    "b <tab>" '("Popup buffers" . popper-cycle)))

(provide 'init-windows)
