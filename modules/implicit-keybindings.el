
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
  (general-define-key :keymaps '(normal visual emacs insert) "M-x" 'counsel-M-x))


;; global keybindings
;; these keybindings are available independent of local buffer mode
;; they can only be overriden by buffer local modes
(+leader-keys
  ":" '("Execute command"                . counsel-M-x)
  "." '("Find file in cwd"               . counsel-find-file)
  "*" '("Find thing-at-point in project" . +search/rg-thing-at-point)
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
  "c r" '("LSP Rename" . lsp-rename)
  "c d" '("LSP Describe thing at point" . lsp-describe-thing-at-point)

  ;; dired actions
  "d" '(:ignore t :which-key "dired")

  ;; file operations
  "f" '(:ignore t :which-key "find")
  "f P" '("Browse modules" . +modules/browse)
  "f R" '("Rename current file" . rename-visited-file)
  "f r" '("Recent files" . counsel-recentf)

  ;; git and version control generally
  "g" '(:ignore t :which-key "git")
  "g d" '("Show diff" . diff-hl-show-hunk)
  "g g" '("Magit" . magit)

  ;; documentation lookup
  "h" '(:ignore t :which-key "help")
  "h b" '("Describe bindings" . describe-bindings)
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

  ;; notes, mostly org
  "n" '(:ignore t :which-key "notes")
  "n r" '(:ignore t :which-key "org roam")
  "n r f" '("Find Org Roam node" . org-roam-node-find)
  "n r r" '("Find random Org Roam node" . org-roam-node-random)
  "n r s" '("Sync Org Roam database" . org-roam-db-sync)
  "n r c" '("Org Roam capture" . org-roam-capture)

  ;; opening important side windows and buffers
  "o" '(:ignore t :which-key "open")
  "o x" '("Scratch buffer" . scratch-buffer)
  "o t" '("Terminal" . vterm)
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
  "p T" '("Test project"                . projectile-test-project)

  ;; quitting and restarting emacs
  "q" '(:ignore t :which-key "quit")
  "q A" '("Save all and kill emacs" . save-buffers-kill-emacs)
  "q r" '("Restart emacs" . restart-emacs)

  ;; searching buffer, dir or project
  "s" '(:ignore t :which-key "search")
  "s b" '("Swiper" . swiper)
  "s i" '("imenu" . imenu)
  ;; toggling features on and off
  "t" '(:ignore t :which-key "toggle")
  "t c" '("Display colors" . rainbow-mode)
  "t v" '("Visual line mode" . visual-line-mode)

  ;; window management
  "w" '(:ignore t :which-key "window")
  "w p" '("Popup window" . popwin:popup-window)
  "w s" '("Split window horizontally" . evil-window-split)
  "w w" '("Switch" . ace-window)
  "w v" '("Split window vertically" . evil-window-vsplit)
  
  "X" '("Org capture" . org-capture))

;; global keybindings
(general-def
  "C-x k" 'kill-this-buffer
  "M-x" '(lambda ()
	    (interactive)
	    (counsel-M-x ""))
  "C-SPC" 'completion-at-point
  "C-c p" '("Projectile commands" . projectile-command-map))
;; local mode map keybindings
;;###################################################

(general-defs
  ;; globals
  :states '(visual normal)
  "g c" 'evilnc-comment-or-uncomment-lines
  "<escape>" 'keyboard-quit
  evil-normal-state-map
  "K" 'helpful-at-point
  
  ;; calibre
  calibredb-search-mode-map
  :states '(normal)
  "q" 'calibredb-search-quit
  
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
  "C-h" 'corfu-doc-toggle
  "C-d" 'evil-lookup-func
  "<tab>" 'corfu-next
  "<backtab>" 'corfu-previous
  corfu-candidate-overlay-map
  "<tab>" 'corfu-candidate-overlay-complete-at-point
  
  ;; dired
  dired-mode-map
  :states '(normal visual)
  "RET" 'dired-find-file
  "S-RET" 'dired-find-file-other-window
  "!"	 'dired-do-shell-command
  "#"	 'dired-flag-auto-save-files
  "$"	 'dired-hide-subdir
  "&"	 'dired-do-async-shell-command
  "("	 'dired-hide-details-mode
  "+"	 'dired-create-directory
  "."	 'dired-clean-directory
  "<"	 'dired-prev-dirline
  "="	 'dired-diff
  ">"	 'dired-next-dirline
  "?"   'dired-summary
  "A"	 'dired-do-find-regexp
  "B"	 'dired-do-byte-compile
  "C"   'dired-do-copy
  "D" 	 'dired-do-delete
  "G"	 'dired-do-chgrp
  "H"	 'dired-do-hardlink
  "I"	 'dired-do-info
  "L"	 'dired-do-load
  "M"	 'dired-do-chmod
  "N" 	 'dired-do-man
  "O"	 'dired-do-chown
  "P"	 'dired-do-print
  "Q"	 'dired-do-find-regexp-and-replace
  "R"	 'dired-do-rename
  "S"	 'dired-do-symlink
  "T"	 'dired-do-touch
  "U"	 'dired-unmark-all-marks
  "W"	 'browse-url-of-dired-file
  "X"	 'dired-do-shell-command
  "Y"	 'dired-do-relsymlink
  "Z"	 'dired-do-compress
  "^"	 'dired-up-directory
  "a"	 'dired-find-alternate-file
  "c"	 'dired-do-compress-to
  "d"	 'dired-flag-file-deletion
  "o"   'dired-do-redisplay
  "m"	 'dired-mark
  "j"	 'dired-next-line
  "k"	 'dired-previous-line
  "q"	 'quit-window
  "s"	 'dired-sort-toggle-or-edit
  ;; "s"   '+dired/choose-sort
  "t"	 'dired-toggle-marks
  "u"	 'dired-unmark
  "v"	 'dired-view-file
  "w"	 'dired-copy-filename-as-kill
  "x"	 'dired-do-flagged-delete
  "i"	 'dired-show-file-type
  "-"   'dired-up-directory
  
  ;; helpful docs
  helpful-mode-map
  :states '(normal)
  "q" 'quit-window
  
  ;; lsp
  lsp-mode-map
  :states '(normal)
  "K" 'lsp-describe-thing-at-point
  lsp-ui-mode-map
  :states '(normal visual)
  :prefix "SPC"
  "t s" '("Toggle sideline display" . (lambda ()
					(interactive)
					(lsp-ui-sideline-mode)))
  "c a" '("Apply code actions" . lsp-ui-sideline-apply-code-actions)
  
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
  "m l" '(:ignore t :which-key "link")
  "m l i" '("Insert link" . org-insert-link)
  org-mode-map
  :states '(normal visual)
  :prefix "SPC n r"
  :global-prefix "M-SPC n r"
  "i" '("Insert Org Roam node" . org-roam-node-insert)
  
  ;; rustic mode
  rustic-mode-map
   :states '(normal visual)
   :prefix "SPC"
   "c r" '("Cargo run" . rustic-cargo-run))


(provide 'implicit-keybindings)
