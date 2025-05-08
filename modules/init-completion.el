;;; -*- lexical-binding: t -*-

(use-package minibuffer
  :straight nil
  :hook
  (minibuffer-mode-hook . visual-line-mode))

(use-package completion-preview
  :straight nil
  :general
  (completion-preview-active-mode-map
   :states '(insert emacs)
   "C-n" 'completion-preview-next-candidate
   "C-p" 'completion-preview-prev-candidate))

(use-package marginalia
  :init
  (setq marginalia-align 'center
        marginalia-align-offset -3)
  :hook
  (after-init-hook . marginalia-mode))

(use-package vertico
  :custom
  (vertico-count 15)
  (vertico-resize nil)
  (vertico-cycle t)
  (vertico-preselect 'first)
  :init
  (setq vertico-scroll-margin 5
        vertico-flat-format
        '(:multiple
          #(" %s " 0 1 (face minibuffer-prompt) 3 4 (face minibuffer-prompt))
          :single
          #("[%s]" 0 1 (face minibuffer-prompt) 1 3 (face success) 3 4
            (face minibuffer-prompt))
          :prompt
          #("(%s)" 0 1 (face minibuffer-prompt) 3 4 (face minibuffer-prompt))
          :separator #(" | " 0 3 (face minibuffer-prompt)) :ellipsis
          #(" ... " 0 1 (face minibuffer-prompt)) :no-match "[No match]" :spacer
          #(" " 0 1 (cursor t))))
  :config
  (vertico-flat-mode 1)
  :hook
  (marginalia-mode-hook . vertico-mode)
  :general
  (+leader-keys
    "t c" '("Bring up last completion" . vertico-suspend))
  (vertico-map
   "C-c f" '("Toggle flat mode" . vertico-flat-mode)
   "C-c ." '("Repeat last vertico session" . vertico-repeat)
   "C-c i" '("Insert candidate" . vertico-insert)
   "C-c s" '("Suspend current session" . vertico-suspend)
   "C-c b" '("Toggle vertico-buffer-mode" . vertico-buffer-mode)))

(use-package embark-consult)

(use-package embark
  :custom
  (embark-mixed-indicator-delay 0.3)
  (embark-prompter 'embark-keymap-prompter)
  (embark-quit-after-action t)
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

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)
  :config
  (require 'embark-consult)
  :general
  (vertico-map
   "C-c e" '("Embark export" . embark-export)
   "C-c b" '("Embark become" . embark-become)
   "C-c l" '("Embark live" . embark-live)
   "C-c o" '("Embark open" . embark-open-externally)
   "C-c a" '("Embark act" . embark-act)
   "C-c A" '("Embark act on all" . embark-act-all)
   "C-c d" '("Embark dwim" . embark-dwim)
   "C-c i" '("Embark insert" . embark-insert)
   "C-c c" '("Embark collect" . embark-collect))
  (general-override-mode-map
   :states '(normal visual)
   "C-c a" 'embark-act
   "C-c A" 'embark-act-all)
  (+leader-keys
    "e" '("Embark Act" . embark-act)))

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
   :initial "")
  :general
  (rg-global-map
   :states '(normal visual)
   "?" 'consult-ripgrep)
  (+leader-keys
    "m :" '("Run active mode command" . consult-mode-command)
    "/" '("Grep directory" . consult-ripgrep)
    ;; buffer
    "b b" '("Find buffer" . consult-buffer)
    "c e" '("Compile errors" . consult-compile-error)
    ;; file
    "f r" '("Recent files" . consult-recent-file)
    "f /" '("Find files" . consult-find)
    "h i" '("Emacs Info" . consult-info)
    "h ! m" '("Manpages" . consult-man)
    "o b" '("Bookmarks" . consult-bookmark)
    ;; search
    "s b" '("Search buffer" . consult-line)
    "s B" '("Search all buffers" . consult-line-multi)
    "s i" '("Imenu" . consult-imenu)
    "s I" '("imenu everywhere" . consult-imenu-multi)
    "s o" '("Imenu outline" . consult-outline)
    "t m" '("Toggle minor mode" . consult-minor-mode-menu)))


(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

(+leader-keys
  ":" '("Execute command" . execute-extended-command)
  "h l" '("Load library" . load-library))

(provide 'init-completion)
