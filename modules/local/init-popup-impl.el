(require 'popup)
(require 'init-utils)

(defvar popup-border-chars '((horizontal . ?―)
                             (vertical . ?|)
                             (top-left . ?⌜)
                             (top-right . ?⌝)
                             (bottom-left . ?⌞)
                             (bottom-right . ?⌟)))

(defcustom popup-strip-props-modes '(lsp-mode)
  "List of modes in which to strip text properties form documentation\
before displaying it in the popup."
  :type '(repeat symbol) )

(cl-defun +popup-menu--completing-read-default-lookup (choice list)
  choice)

;;;###autoload
(cl-defun +popup-menu--completing-read (collection
                                        &key
                                        prompt
                                        (lookup '+popup-menu--completing-read-default-lookup))
  (funcall lookup (popup-menu* collection
                               :prompt prompt
                               :help-delay nil
                               :margin 1
                               :point (point)
                               :isearch t)))

;;;###autoload
(defun +eldoc-display-in-popup (docs interactive)
  "Eldoc display function to display documentation in a popup."
  (let* ((body (mapconcat (lambda (doc)
                            (let* ((text (car doc))
                                   (rest (cdr doc))
                                   (echo (plist-get rest :echo))
                                   (thing (plist-get rest :thing))
                                   (thing (when thing
                                            (popup-x-to-string thing))))
                              (if echo
                                  echo
                                (when text
                                  (concat (when thing (concat thing ": ")) text)))))
                          docs
                          "\n\n")))
    (when body
      (let ((strip (eval `(or ,@(mapcar (lambda (sym)
                                          (when (bound-and-true-p sym)
                                            t))
                                        popup-strip-props-modes)))))
        (popup-tip body
                   :truncate nil
                   :margin 1
                   :around t
                   :nostrip (any-bound-and-true popup-strip-props-modes)
                   :max-width (if interactive 100 60)
                   :max-height (if interactive 80 20)
                   :scroll-bar nil)))))


(provide 'iniit-popup-impl)
