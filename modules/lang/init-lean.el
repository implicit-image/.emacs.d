;;; -*- lexical-binding: t -*-

;;;; support for legacy Lean3 theorem prover
(use-package lean-mode)

;;;; support for Lean4 theorem prover
(use-package lean4-mode
  :commands lean4-mode
  :straight (lean4-mode :type git :host github
			:repo "leanprover-community/lean4-mode"
			:files ("*.el" "data")))

(provide 'init-lean)
