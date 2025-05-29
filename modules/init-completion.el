;;; -*- lexical-binding: t -*-

;; (use-package minibuffer
;;   :straight nil
;;   :hook
;;   (minibuffer-mode-hook . visual-line-mode))

(add-hook 'minibuffer-mode-hook 'visual-line-mode)

;; (use-package completion-preview
;;   :straight nil)
;; :bind*
;; ( :map completion-preview-active-mode-map
;;   ("C-n" . completion-preview-next-candidate)
;;   ("C-p" . completion-preview-prev-candidate)))
;; :general
;; (completion-preview-active-mode-map
;;  :states '(insert emacs)
;;  "C-n" 'completion-preview-next-candidate
;;  "C-p" 'completion-preview-prev-candidate))

(use-package marginalia
  :custom
  (marginalia-align 'center)
  (marginalia-align-offset -3)
  :hook
  (after-init-hook . marginalia-mode))

(use-package vertico
  :custom
  (vertico-count 13)
  (vertico-resize nil)
  (vertico-cycle t)
  (vertico-preselect 'first)
  (vertico-scroll-margin 5)
  :hook
  (marginalia-mode-hook . vertico-mode))
;; :bind*
;; ( :map vertico-map
;;   ("C-c f" . vertico-flat-mode)
;;   ("C-c ." . vertico-repeat)
;;   ("C-c i" . vertico-insert)))
;; :general
;; (+leader-keys
;;   "t c" '("Bring up last completion" . vertico-suspend))
;; (vertico-map
;;  "C-c f" '("Toggle flat mode" . vertico-flat-mode)
;;  "C-c ." '("Repeat last vertico session" . vertico-repeat)
;;  "C-c i" '("Insert candidate" . vertico-insert)
;;  "C-c s" '("Suspend current session" . vertico-suspend)))

(use-package embark-consult)

(use-package embark
  :custom
  (embark-mixed-indicator-delay 0.3)
  (embark-prompter 'embark-keymap-prompter)
  (embark-quit-after-action t)
  (embark-indicators '(embark-which-key-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :init
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)
  :config
  (require 'embark-consult))
;; :bind*
;; (("C-x a" . embark-act)
;;  ("C-x SPC a" . embark-act))
;; :bind*
;; ( :map vertico-map
;;   ("C-c e" . embark-export)
;;   ("C-c b" . embark-become)
;;   ("C-c l" . embark-live)
;;   ("C-c o" . embark-open-externally)
;;   ("C-c a" . embark-act)
;;   ("C-c A" . embark-act-all)
;;   ("C-c d" . embark-dwim)
;;   ("C-c i" . embark-insert)
;;   ("C-c c" . embark-collect)))

(use-package consult
  :autoload
  (consult--read)
  :custom
  (xref-show-xrefs-function 'consult-xref)
  :init
  (setq consult-find-args (+os/per-system! :wsl  (format "%s . -not ( -path */.[A-Za-z]* -prune )"
                                                         find-program)
                                           :linux (format "%s . -not ( -path */.[A-Za-z]* -prune )"
                                                          find-program)
                                           :win (format "%s . -not ( -path */.[A-Za-z]* -prune )"
                                                        find-program)))

  :config
  (consult-customize
   consult-grep consult-ripgrep consult-git-grep consult-line-multi
   :initial ""))
;; :bind*
;; (("C-x C-m :" . consult-mode-command)
;;  ("C-x C-/" . consult-ripgrep)
;;  ("C-x C-b b" . consult-buffer)
;;  ("C-x C-c" .  consult-compile-error)
;;  ("C-x C-f r" . consult-recent-file)
;;  ("C-x C-f /" . consult-find)
;;  ("C-x C-h i" . consult-info)
;;  ("C-x C-h ! m" . consult-man)
;;  ("C-x C-o " . consult-bookmark)
;;  ("C-x C-s b" . consult-line)
;;  ("C-x C-s B" . consult-line-multi)
;;  ("C-x C-s i" . consult-imenu)
;;  ("C-x C-s I" . consult-imenu-multi)
;;  ("C-x C-s o" . consult-outline)
;;  ("C-x C-t m" . consult-minor-mode-menu)
;;  :map org-mode-map
;;  ("M-SPC s i" . consult-org-heading)
;;  :map rg-global-map
;;  ("?" . consult-ripgrep)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

(provide 'init-completion)
