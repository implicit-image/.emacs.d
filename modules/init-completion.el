;;; -*- lexical-binding: t -*-
(use-package company
  :disabled t)

;; disable loading company if something tries
(defalias 'company-mode 'ignore)

(use-package ivy
  :custom-face
  (ivy-current-match ((t (:background ,(doom-lighten (doom-color 'selection) 0.1)))))
  :init
  (setq ivy-height 15
	ivy-fixed-height-minibuffer t
	ivy-use-virtual-buffers t
	enable-recursive-minibuffers t
	ivy-initial-inputs-alist '())
  :config
  (ivy-mode)
  :hook (after-init . ivy-mode))

(use-package counsel
  :config
  ;; dont use package.el at all
  (defalias 'counsel-package 'ignore)
  :general
  (+leader-keys
    ":" '("Execute command" . counsel-M-x)
    "." '("Find file in cwd" . counsel-find-file)
    "/" '("Search project" . counsel-rg)
    ;; buffer
    "b b" '("Find buffer" . counsel-switch-buffer)
    ;; code
    "c c" '("Compile" . counsel-compile)
    ;; file
    "f r" '("Recent files" . counsel-recentf)
    ;; help
    "h l" '("Load library" . counsel-load-library)
    "h F" '("Describe face" . counsel-faces)
    "h s" '("Describe symbol" . counsel-describe-symbol)
    "h t" '("Load theme" . counsel-load-theme)
    ;; insert
    "i u" '("Unicode char" . counsel-unicode-char)
    "i c" '("Color hexstring" . counsel-colors-web)
    ;; search
    "s i" '("imenu" . counsel-imenu))
  (general-def general-override-mode-map
    :states '(normal visual emacs insert)
    "M-x" 'counsel-M-x))

(use-package swiper
  :general
  (+leader-keys
    "s b" '("Swiper" . swiper)
    "s B" '("swiper thing at point" . swiper-thing-at-point)))

(use-package marginalia
  :demand
  :init
  (setq marginalia-align 'center
	marginalia-align-offset -3)
  :config
  (marginalia-mode +1))

;;################################################################
;; Embark + consult + vertico stack
;; disabled for now
;; NOTE: maybe replace ivy+counsel in the future
(use-package embark
  :disabled
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


(use-package consult
  :disabled
  :config
  :general
  (+leader-keys
    ":" '("Execute command" . execute-extended-command)
    "." '("Find file in cwd" . find-file)
    "/" '("Search project" . consult-)
    ;; buffer
    "b b" '("Find buffer" . counsel-switch-buffer)
    ;; code
    "c c" '("Compile" . counsel-compile)
    ;; file
    "f r" '("Recent files" . counsel-recentf)
    ;; help
    "h l" '("Load library" . counsel-load-library)
    "h F" '("Describe face" . counsel-faces)
    "h s" '("Describe symbol" . counsel-describe-symbol)
    "h t" '("Load theme" . counsel-load-theme)
    ;; insert
    "i u" '("Unicode char" . counsel-unicode-char)
    "i c" '("Color hexstring" . counsel-colors-web)
    ;; search
    "s i" '("imenu" . counsel-imenu))
  (general-def general-override-mode-map
    :states '(normal visual emacs insert)
    "M-x" 'counsel-M-x))

(use-package vertico
  :disabled
  :custom
  (vertico-count 13)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  :config
  (vertico-mode))


(use-package embark-consult
  :disabled
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; #########################################################

;; in-buffer completion

(use-package yasnippet-capf
  :after doom-snippets
  :config
  (add-hook 'completion-at-point-functions #'yasnippet-capf))

(use-package tempel
  :after cape)

(use-package tempel-collection
  :after tempel)

(use-package nerd-icons-corfu
  :functions
  nerd-icons-corfu-formatter
  :config
  (setq nerd-icons-corfu-mapping
	'((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
	  (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
	  (class :style "cod" :icon "symbol_class" :face font-lock-type-face)
	  (color :style "fae" :icon "palette_color" :face success)
	  (command :style "dev" :icon "terminal_badge" :face default)
	  (constant :style "cod" :icon "symbol_constant" :face font-lock-constant-face)
	  (constructor :style "cod" :icon "triangle_right" :face font-lock-function-name-face)
	  (enummember :style "cod" :icon "symbol_enum_member" :face font-lock-builtin-face)
	  (enum-member :style "cod" :icon "symbol_enum_member" :face font-lock-builtin-face)
	  (enum :style "cod" :icon "symbol_enum" :face font-lock-builtin-face)
	  (event :style "cod" :icon "symbol_event" :face font-lock-warning-face)
	  (field :style "cod" :icon "symbol_field" :face font-lock-variable-name-face)
	  (file :style "fa" :icon "file_o" :face font-lock-string-face)
	  (folder :style "md" :icon "folder" :face font-lock-doc-face)
	  (interface :style "cod" :icon "symbol_interface" :face font-lock-type-face)
	  (keyword :style "cod":icon "symbol_keyword" :face font-lock-keyword-face)
	  (macro :style "md" :icon "exclamation_thick" :face font-lock-keyword-face)
	  (magic :style "cod" :icon "wand" :face font-lock-builtin-face)
	  (method :style "md" :icon "lambda" :face font-lock-number-face)
	  (function :style "md" :icon "lambda" :face font-lock-function-name-face)
	  (module :style "oct" :icon "file_submodule" :face font-lock-preprocessor-face)
	  (numeric :style "md" :icon "umeric" :face font-lock-builtin-face)
	  (operator :style "cod" :icon "symbol_operator" :face font-lock-comment-delimiter-face)
	  (param :style "cod" :icon "symbol_parameter" :face default)
	  (property :style "cod" :icon "symbol_property" :face font-lock-variable-name-face)
	  (reference :style "cod" :icon "references" :face font-lock-variable-name-face)
	  (snippet :style "fa" :icon "code" :face font-lock-string-face)
	  (string :style "oct" :icon "quote" :face font-lock-string-face)
	  (struct :style "md" :icon "code_braces" :face font-lock-variable-name-face)
	  (text :style "cod" :icon "text_size" :face font-lock-doc-face)
	  (typeparameter :style "oct" :icon "list_unordered" :face font-lock-type-face)
	  (type-parameter :style "oct" :icon "list_unordered" :face font-lock-type-face)
	  (unit :style "cod" :icon "symbol_ruler" :face font-lock-constant-face)
	  (value :style "cod" :icon "symbol_field" :face font-lock-builtin-face)
	  (variable :style "md" :icon "variable" :face font-lock-variable-name-face)
	  (t :style "oct" :icon "code_square" :face font-lock-warning-face)))
  :hook
  (global-corfu-mode . (lambda ()
			 (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))))

(general-def global-map
  "C-SPC" 'completion-at-point)

(provide 'init-completion)
