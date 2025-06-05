;;; -*- lexical-binding: t -*-

(setq project-buffers-viewer 'project-list-buffers-consult
      project-vc-ignores '("straight/")
      project-vc-extra-root-markers (append project-vc-extra-root-markers
                                            '("package.json" "cargo.toml" ".+\.cabal")))

(add-hook 'project-find-functions '+project--git-root-finder)
;; to fix weird slow down
(advice-add 'project-try-vc :override 'ignore)

(use-package direnv
  :if (+os/is-linux-p)
  :init
  (setq direnv-always-show-summary nil
        direnv-show-paths-in-summary nil)
  :hook
  (after-init-hook . direnv-mode))

(provide 'init-projects)
