


;; get test bench block
;; parse it to temp file
;; ghdl 


;; (defun +vhdl/parse-unit-names (body)
;;   ""
;;    (cl-remove-if #'not (mapcar (lambda (str)
;; 	     (when (string-prefix-p "entity" str)
;; 		 (string-trim-right (string-trim-left str "entity ") " is")))
;; 	   (string-split body "\n"))))

;; (defun +vhdl/get-unit-names (buffer)
;;   "Return all lowercae names of units defined in BUFFER."
;;   (+vhdl/parse-unit-names (+utils/whole-buffer-as-string buffer)))




;; (defun org-babel-execute:vhdl (body params)
;;   (let* ((tmp-design-file (org-babel-temp-file "VHDL-design-" ".vhdl"))
;; 	 (tmp-test-bench-file (org-babel-temp-file "VHDL-tb-" ".vhdl"))
;; 	 (unit-names (car (+vhdl/parse-unit-names body)))
;; 	 (test-bench-src-block-name (org-babel-get-src-block-info params))
;; 	 )
;;     (with-temp-file tmp-design-file (insert body))
;;     (orb-babel-eval
;;      (format "%s -a %s"
;; 	     vhdl-compiler
;; 	     (org-babel-process-file-name tmp-design-file))
;;      "")
;;     (let ((results)))
    
;;     ))


;; (defun org-babel-execute:nand2tetris (params body)
;;   (let* ((out-file (cdr (or (assq :file params)
;; 			    (error "You need to specify a :file parameter"))))
;; 	 (cmdline (or (cdr (assq :cmdline params))
;; 		      (format "-T%s" (file-name-extension out-file))))
;; 	 (cmd (or (cdr (assq :cmd params)) "dot"))
;; 	 (coding-system-for-read 'utf-8) ;use utf-8 with sub-processes
;; 	 (coding-system-for-write 'utf-8)
;; 	 (in-file (org-babel-temp-file "dot-")))
;;     (with-temp-file in-file
;;       (insert (org-babel-expand-body:dot body params)))
;;     (org-babel-eval
;;      (concat cmd
;; 	     " " (org-babel-process-file-name in-file)
;; 	     " " cmdline
;; 	     " -o " (org-babel-process-file-name out-file)) "")
;;     nil) 


(use-package nand2tetris
  :mode "\\.hdl\\'"
  :straight (nand2tetris :type git
			 :host github
			 :repo "CestDiego/nand2tetris.el")
  :config
  (setq nand2tetris-core-base-dir "~/projects/nand2tetris/core"))



(provide 'implicit-hdl)
