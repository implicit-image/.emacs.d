;;; -*- lexical-binding: t -*-

(use-package marginalia
  :init
  (setq marginalia-align 'center
	marginalia-align-offset -3)
  :hook
  (after-init . marginalia-mode))

(use-package vertico
  :custom
  (vertico-count 15)
  (vertico-resize nil)
  (vertico-cycle t)
  (vertico-preselect 'first)
  :init
  (setq vertico-scroll-margin 5)
  :hook
  (marginalia-mode . vertico-mode))

(use-package embark
  :custom
  (embark-mixed-indicator-delay 0.3)
  :init
  (+windows-cfg '(("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*")
		  :regexp t
		  :position bottom
		  :height 0.3
		  :dedicated nil
		  :noselect nil))
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package consult
  :autoload
  (consult--read)
  :custom
  (xref-show-xrefs-function 'consult-xref)
  :config
  (consult-customize
   consult-grep consult-ripgrep consult-git-grep consult-line-multi
   :initial "")
  :general
  (org-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "s i" '("Org heading" . consult-org-heading)
   "s o" '("Outline" . consult-outline))
  (markdown-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "s o" '("Outline" . consult-outline))
  (+leader-keys
    "m :" '("Run active mode command" . consult-mode-command)
    "/" '("Search directory" . consult-ripgrep)
    ;; buffer
    "b b" '("Find buffer" . consult-buffer)
    "c e" '("Compile errors" . consult-compile-error)
    ;; file
    "f r" '("Recent files" . consult-recent-file)
    "h i" '("Emacs Info" . consult-info)
    "o b" '("Bookmarks" . consult-bookmark)
    ;; search
    "s b" '("Search buffer" . consult-line)
    "s B" '("Search all buffers" . consult-line-multi)
    "s i" '("imenu" . consult-imenu)
    "s I" '("imenu everywhere" . consult-imenu-multi)
    "t m" '("Toggle minor mode" . consult-minor-mode-menu)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

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

(+leader-keys
  ":" '("Execute command" . execute-extended-command)
  "." '("Find file in cwd" . find-file)
  "c c" '("Compile" . compile)
  "h l" '("Load library" . load-library)
  "h F" '("Describe face" . describe-face)
  "h s" '("Describe symbol" . describe-symbol)
  "h t" '("Load theme" . load-theme))

(provide 'init-completion)
