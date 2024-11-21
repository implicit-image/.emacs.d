(use-package hydra)

(use-package which-key
  :demand
  :commands
  (which-key-setup-side-window-bottom)
  :init
  (setq which-key-popup-type 'minibuffer
	which-key-idle-delay 0.6
	which-key-max-display-columns 5
	which-key-add-column-padding 10
	which-key-min-display-lines 5)
  (which-key-setup-side-window-bottom)
  :config
  (which-key-mode))

(use-package general
  :demand
  :config
  (general-evil-setup)
  (general-create-definer +leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")
  (general-create-definer +local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC m"
    :global-prefix "M-SPC m")
  ;; replace M-x
  (general-define-key :keymaps '(normal visual emacs insert) "M-x" 'counsel-M-x)

  ;; global keybindings with leader key
  ;; these keybindings are available independent of local buffer mode
  ;; they can only be overriden by buffer local modes
  (+leader-keys
    ":" '("Execute command"                . counsel-M-x)
    "." '("Find file in cwd"               . counsel-find-file)
    "*" '("Find thing-at-point in project" . +search/rg-thing-at-point)
    "TAB" '("Switch to project tree" . treemacs-select-window)
    "SPC" '("Find file in project"         . projectile-find-file)
    "/" '("Search project"                 . counsel-rg)
    "`" '("Last buffer"                    . evil-switch-to-windows-last-buffer)
    ;; launching client modes
    "a" '(:ignore t :which-key "applications")
    "a c" '("Open calibre" . calibredb)

    ;; buffer operations
    "b" '(:ignore t :which-key "buffer")
    "b r" '("Revert" . revert-buffer)
    "b i" '("Open ibuffer"     . ibuffer)
    "b b" '("Find buffer"      . counsel-switch-buffer)
    "b K" '("Kill this buffer" . kill-this-buffer)
    "b k" '("Kill buffer"      . kill-buffer)
    "b p" '("Popup buffer" . popwin:popup-buffer)

    ;; code editing and lsp functions
    "c" '(:ignore t :which-key "code")
    "c R" '("Rename" . anzu-query-replace)
    ;; "c k" '("Describe thing at point" . lsp-describe-thing-at-point)
    "c c" '("Compile" . counsel-compile)
    "c d" '("Goto definition" . evil-goto-definition)
    "c r" '("Goto references" . xref-find-references)

    ;; dired actions
    "d" '(:ignore t :which-key "dired")

    ;; file operations
    "f" '(:ignore t :which-key "find")
    "f D" '("Delete current file" . +utils/delete-visited-file)
    "f o" '("Find file in other window" . find-file-other-window)
    "f P" '("Browse modules" . +modules/browse)
    "f R" '("Rename current file" . rename-visited-file)
    "f r" '("Recent files" . counsel-recentf)
    "f s" '("Open as sudo" . +remote/open-file-as-sudo)
    "f S" '("Open remote file" . +remote/counsel-find-file)

    ;; git and version control generally
    "g" '(:ignore t :which-key "git")
    "g g" '("Magit" . magit)

    ;; documentation lookup
    "h" '(:ignore t :which-key "help")
    "h b" '("Describe bindings" . describe-bindings)
    "h d" '("Dictionary" . dictionary-search)
    "h t" '("Describe font"     . describe-font)
    "h l" '("Load library"      . counsel-load-library)
    "h m" '("Describe keymap"   . describe-keymap)
    "h M" '("Describe mode"     . describe-mode)
    "h F" '("Describe face"     . counsel-faces)
    "h p" '("Describe package"  . describe-package)
    "h s" '("Describe symbol"   . counsel-describe-symbol)
    "h t" '("Load theme"        . counsel-load-theme)
    "h v" '("Describe variable" . helpful-variable)
    "h f" '("Describe function" . helpful-callable)
    "h k" '("Describe key" . helpful-key)

    ;; choosing and inserting data
    "i" '(:ignore t :which-key "insert")
    "i u" '("Unicode char" . counsel-unicode-char)
    "i c" '("Color hexstring" . counsel-colors-web)

    ;; notes, mostly org
    "n" '(:ignore t :which-key "notes")
    "n r" '(:ignore t :which-key "org roam")
    "n r f" '("Find Org Roam node" . org-roam-node-find)
    "n r r" '("Find random Org Roam node" . org-roam-node-random)
    "n r s" '("Sync Org Roam database" . org-roam-db-sync)
    "n r c" '("Org Roam capture" . org-roam-capture)
    "n r *" '("Grep in org roam dir" . +org-roam/rg-in-notes)

    ;; opening important side windows and buffers
    "o" '(:ignore t :which-key "open")
    "o x" '("Scratch buffer" . scratch-buffer)
    "o T" '("Popup terminal" . +terminal/open)
    "o t" '(:ignore t :which-key "Terminal")
    "o t r" '("Run command" . +terminal/run-command)
    "o t c" '("Switch to other" . +terminal/counsel-vterm)
    "o p" '("Sidebar" . treemacs)
    "o u" '("Undo tree" . undo-tree-visualize)
    "o A" '("Org agenda" . org-agenda)
    "o a" `("Agenda file" . (lambda ()
			      (interactive)
			      (org-open-file +org/agenda-file)))
    ;; projectile functions
    "p" '(:ignore t :which-key "projects")
    "p !" '("Run cmd in project root"     . projectile-run-shell-command-in-root)
    "p &" '("Async cmd in project root"   . projectile-run-async-shell-command-in-root)
    "p ." '("Browse project"              . projectile-find-file)
    "p >" '("Browse other project"        . projectile-find-other-file)
    "p a" '("Add new project"             . projectile-add-known-project)
    "p b" '("Switch to project buffer"    . projectile-switch-to-buffer)
    "p c" '("Compile in project"          . projectile-compile-project)
    "p C" '("Repeat last command"         . projectile-repeat-last-command)
    "p d" '("Remove known project"        . projectile-remove-known-project)
    "p D" '("Discover projects in folder" . projectile-discover-projects-in-directory)
    "p e" '("Edit project's .dir-locals"  . projectile-edit-dir-locals)
    "p f" '("Find file in project"        . projectile-find-file)
    "p F" '("Find file in other project"  . projectile-find-other-file)
    "p g" '("Configure project"           . projectile-configure-project)
    "p i" '("Invalidate project cache"    . projectile-invalidate-cache)
    "p k" '("Kill project buffers"        . projectile-kill-buffers)
    "p o" '("Find sibling file"           . projectile-find-related-file)
    "p p" '("Switch project"              . projectile-switch-project)
    "p r" '("Find recent project files"   . projectile-recentf-files)
    "p R" '("Run project"                 . projectile-run-project)
    "p s" '("Save project files"          . projectile-save-project-buffers)
    "p t" '("List project todos"          . magit-todos-list)
    "p T" '("Project terminal"                . projectile-run-vterm)

    ;; quitting and restarting emacs
    "q" '(:ignore t :which-key "quit")
    "q A" '("Save all and kill emacs" . save-buffers-kill-emacs)
    "q r" '("Restart emacs" . restart-emacs)

    ;; searching buffer, dir or project
    "s" '(:ignore t :which-key "search")
    "s b" '("Swiper" . swiper)
    "s B" '("swiper thing at point" . swiper-thing-at-point)
    "s i" '("imenu" . counsel-imenu)
    "s r" '("Query replace" . anzu-query-replace)
    "s R" '("Query replace regexp" . anzu-query-replace-regexp)

    ;; toggling features on and off
    "t" '(:ignore t :which-key "toggle")
    "t a" '("Toggle autocompletion" . (lambda ()
					(interactive)
					(setq lsp-bridge-complete-manually (not lsp-bridge-complete-manually))))
    "t A" '("Reload autocompletion config" . +lsp/reload-autocompletion)
    "t c" '("Colorize color strings" . rainbow-mode)
    "t I" '("Select input method" . set-input-method)
    "t m" '("Toggle minor mode" . +utils/counsel-toggle-minor-mode)
    "t v" '("Visual line mode" . visual-line-mode)

    ;; window management
    "w" '(:ignore t :which-key "window")
    "w s" '("Split window horizontally" . evil-window-split)
    "w w" '("Switch" . ace-window)
    "w v" '("Split window vertically" . evil-window-vsplit)

    "X" '("Org capture" . org-capture))

  (general-defs
    general-override-mode-map
    :states '(normal visual)
    "C-h" 'evil-window-left
    "C-j" 'evil-window-down
    "C-k" 'evil-window-up
    "C-l" 'evil-window-right
    ;; globals
    global-map
    "C-TAB" 'evil-jump-forward
    "<remap> <evil-window-next>" 'ace-window
    "C-=" 'text-scale-increase
    "C--" 'text-scale-decrease
    ;; window navigation
    "C-x w" 'ace-window
    "C-x k" 'kill-this-buffer
    ;; "M-x" '(lambda ()
    ;; 	   (interactive)
    ;; 	   (counsel-M-x ""))
    "C-SPC" 'completion-at-point
    "C-c p" '("Projectile commands" . projectile-command-map)
    global-map
    :states '(visual normal)
    "g c" 'evilnc-comment-or-uncomment-lines
    "<escape>" 'keyboard-quit
    "K" '+lookup/documentation

    ;; pdf view
    pdf-view-mode-map
    :states '(normal)
    "g n" 'org-noter
    pdf-view-mode-map
    :states '(normal)
    "] ]" 'pdf-view-next-page
    "[ [" 'pdf-view-previous-page
    "j" 'pdf-view-scroll-up-or-next-page
    "k" 'pdf-view-scroll-down-or-previous-page
    "=" 'pdf-view-enlarge
    "-" 'pdf-view-shrink
    "C-=" 'pdf-view-center-in-window
    "q" 'kill-this-buffer

    ;; corfu
    corfu-map
    :states 'insert
    "M-h" 'corfu-doc-toggle
    [tab] 'corfu-next
    "<tab>" 'corfu-next
    "TAB" 'corfu-next
    [backtab] 'corfu-previous
    "<backtab>" 'corfu-previous
    "S-TAB" 'corfu-previous


    ;; corfu-candidate-overlay-map
    ;; :states '(insert)
    ;; "<tab>" 'corfu-candidate-overlay-complete-at-point

    ;; helpful docs
    helpful-mode-map
    :states '(normal)
    "q" 'quit-window

    help-mode-map
    :states '(normal)
    "q" 'quit-window

    diff-hl-mode-map
    :states '(normal visual)
    :prefix "SPC g"
    :global-prefix "M-SPC g"
    "v" 'diff-hl-command-map

    diff-hl-show-hunk-map
    :states '(normal visual)
    "]" 'diff-hl-show-hunk-posframe

    lispy-mode-map
    :states '(insert)
    "<remap> "
    "M-<" 'lispy-move-left
    "M->" 'lispy-move-right
    "M-k" 'lispy-kill-sentence

    ;; diff-hl-show-hunk-map
    ;; :states '(insert)

    ;; eglot
    eglot-mode-map
    :states '(normal visual)
    :prefix "SPC"
    :global-prefix "M-SPC"
    "c R" '("Rename" . eglot-rename)

    eglot-mode-map
    :states '(visual)
    "=" '("Format" . eglot-format)

    ;; lsp-bridge mode
    acm-mode-map
    :states 'insert
    "<tab>" 'acm-select-next
    "M-j" 'acm-select-next
    "<backtab>" 'acm-select-prev
    "M-k" 'acm-select-prev
    "RET" 'acm-complete
    "C-<tab>" 'acm-complete
    "C-f" 'acm-filter
    "M-d" 'acm-doc-toggle
    "<escape>" 'acm-hide

    lsp-bridge-mode-map
    :states '(insert)
    "C-SPC" 'lsp-bridge-popup-complete-menu

    lsp-bridge-mode-map
    :states '(normal visual)
    :prefix "SPC c"
    :global-prefix "M-SPC c"
    "a" '("Code actions" . lsp-bridge-code-action)
    "d" '("Find definition" . lsp-bridge-find-def)
    "D" '("Find definition other window" . lsp-bridge-find-def-other-window)
    "t" '("Find typedef" . lsp-bridge-find-type-def)
    "T" '("Find typedef other window" . lsp-bridge-find-type-def-other-window)
    "f r" '("Return to symbol" . lsp-bridge-find-def-return)
    "i" '("Find implementation" . lsp-bridge-find-impl)
    "I" '("Find implementation other window" . lsp-bridge-find-impl-other-window)
    "r" '("Find references" . lsp-bridge-find-references)
    "k" '("Show doc popup" . lsp-bridge-popup-documentation)
    "K" '("Show doc buffer" . lsp-bridge-show-documentation)
    "p" '(:ignore t :which-key "Process")
    "p R" '("Restart process" . lsp-bridge-restart-process)
    "R" '("Rename" . lsp-bridge-rename)
    "s S" '("LSP imenu" . lsp-bridge-imenu)

    lsp-bridge-mode-map
    :states '(normal visual)
    :prefix "SPC s"
    :global-prefix "M-SPC s"
    "S" '("List symbols" . lsp-bridge-workspace-list-symbol-at-point)
    lsp-bridge-doc-map
    :states '(normal visual)
    "q" 'quit-window
    ;; lsp-bridge diagnostics

    slime-popup-buffer-mode-map
    :states '(normal)
    "q" 'quit-window

    lsp-mode-map
    :states '(normal visual)
    :prefix "SPC"
    :global-prefix "M-SPC"
    "s S" '("Search LSP symbol" . lsp-ivy-workspace-symbol)
    "c d" '("Popup documentation" . lsp-ui-doc-show)

    lsp-ui-mode-map
    :states '(normal visual)
    :prefix "SPC"
    "t s" '("Toggle sideline display" . (lambda ()
					  (interactive)
					  (lsp-ui-sideline-mode)))
    "c a" '("Apply code actions" . lsp-ui-sideline-apply-code-actions)

    ;; compilation mode
    ;; flycheck mode
    ;; flycheck-mode-map
    ;; :prefix

    ;; isearch-mode
    ;; isearch-mode-map
    ;; :states '(normal visual)
    ;; replace searched occurrence
    ;; rename searched symbols
    ;; save and browse searched locations


    ;; counsel mode
    ;; counsel item actions

    ;; ivy mode


    ;; org mode
    org-mode-map
    :states '(normal visual)
    "RET" 'org-return

    org-mode-map
    :states '(normal visual)
    :prefix "SPC"
    :global-prefix "M-SPC"
    "t l" '("Toggle latex preview" . org-latex-preview)
    "t p" '("Toggle pretty symbols" . org-toggle-pretty-entities)
    "m i t" '("Create table" . org-table-create-or-convert-from-region)
    "m l" '(:ignore t :which-key "link")
    "m l i" '("Insert link" . org-insert-link)

    ;; org roam
    org-roam-mode-map
    :states '(normal visual)
    :prefix "SPC n r"
    :global-prefix "M-SPC n r"
    "i" '("Insert Org Roam node" . org-roam-node-insert)

    isearcevil-mc-make-all
    :states '(insert)
    "M-s /" 'evil-mc-make-all-cursors

    calibredb-search-mode-map
    :states '(normal)
    "q" 'quit-window

    Info-mode-map
    :states '(normal)
    ">" 'Info-final-node
    ">" 'end-of-buffer
    "?" 'Info-summary
    "?" 'describe-mode
    "C-M-i" 'Info-prev-reference
    "DEL" 'Info-scroll-down
    "DEL" 'scroll-down-command
    "G" 'Info-goto-node-web
    "H" 'describe-mode
    "I" 'Info-virtual-index
    "L" 'Info-history
    "M-g g" 'goto-line-relative
    "M-n" 'clone-buffer
    "RET" 'Info-follow-nearest-node
    "S" 'Info-search-case-sensitively
    "S-SPC" 'Info-scroll-down
    "S-SPC" 'scroll-down-command
    "SPC" 'Info-scroll-up
    "SPC" 'scroll-up-command
    "T" 'Info-toc
    "TAB" 'Info-next-reference
    "[" 'Info-backward-node
    "]" 'Info-forward-node
    "^" 'Info-up
    "c" 'Info-copy-current-node-name
    "d" 'Info-directory
    "f" 'Info-follow-reference
    "g" 'Info-goto-node
    "h" 'Info-help
    "i" 'Info-index
    "l" 'Info-history-back
    "m" 'Info-menu
    "n" 'Info-next
    "p" 'Info-prev
    "q" 'quit-window
    "r" 'Info-history-forward
    "s" 'Info-search
    "t" 'Info-top-node
    "u" 'Info-up
    "w" 'Info-copy-current-node-name

    evil-treemacs-state-map
    :states '(motion)
    "p" 'treemacs-project-map

    gfm-view-mode-map
    :states 'normal
    "q" 'kill-this-buffer

    ;; LANGUAGE MODE BINDINGS

    ;; mix mode
    ;; mix minor mode
    ;; c-mode
    ;; rustic mode
    ;; rustic-mode-map
    ;; nand2tetris-mode-map
    ))


(provide 'init-keybindings)
