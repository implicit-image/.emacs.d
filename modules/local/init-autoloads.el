(autoload '+appearance-setup-doom-themes "init-appearance-impl")

(autoload '+books/org-noter-init-session "init-books-impl")

(autoload '+checkers/flymake-toggle-eol "init-checkers-impl")
;; (autoload 'flymake-prefix-map "init-checkers-impl" nil nil 'keymap)

(autoload 'embark-which-key-indicator "init-embark-impl")
(autoload 'embark-hide-which-key-indicator "init-embark-impl")

(autoload '+global-corfu-mode--setup "init-corfu-impl")
(autoload '+corfu-toggle-auto "init-corfu-impl")
(autoload '+dired-mode--setup "init-dired-impl")

(autoload '+paren--overlay-function "init-paren-impl")

(autoload '+indent-bars-mode--setup "init-indent-impl")
(autoload '+nwscript-mode--setup "init-nwscript-impl")
(autoload '+csv-mode--setup "init-csv-impl")
(autoload '+merlin-mode--setup "init-ocaml-impl")

(autoload '+lookup/documentation "init-lookup-impl")

(autoload 'backward-line "init-meow-impl")
(autoload 'backward-symbol "init-meow-impl")
(autoload '+meow/command "init-meow-impl")
(autoload '+meow/select-for-mc-string "init-meow-impl")
(autoload '+meow/select-for-mc-regex "init-meow-impl")
(autoload '+meow/surround-thing "init-meow-impl")
(autoload '+meow/transpose "init-meow-impl")

(autoload '+mode-line--vc-update "init-vc-impl")

(autoload '+project-list-buffers-consult "init-projects-impl")
(autoload '+project--git-root-finder "init-projects-impl")

(autoload '+search/rg-thing-at-point "init-search-impl")
(autoload '+next-error-no-select "init-search-impl")
(autoload '+previous-error-no-select "init-search-impl")
(autoload '+deadgrep-display-buffer-function "init-search-impl")

(autoload '+treemacs--setup "init-treemacs-impl")

(autoload '+tty-setup "init-tty-impl")

(autoload '+org-roam-mode--setup "init-org-impl")
(autoload '+org-babel--setup "init-org-impl")
(autoload '+org-template-fn--file-in-subdir "init-org-impl")
(autoload '+org/rg-in-roam-notes "init-org-impl")
(autoload '+org-mode--jupyter-setup "init-org-impl")

(autoload '+windows/shift-left "init-windows-impl")
(autoload '+windows/shift-down "init-windows-impl")
(autoload '+windows/shift-up "init-windows-impl")
(autoload '+windows/shift-right "init-windows-impl")

(autoload '+eldoc-display-in-popup "init-popup-impl")
(autoload '+popup-menu--completing-read "init-popup-impl")

(autoload '+lsp-apply-code-action-in-popup "init-lsp-mode-impl")

;; TODO: implement this
;; (autoload '+gptel-get-api-key "init-gptel-impl")

(provide 'init-autoloads)
