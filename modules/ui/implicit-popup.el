;;; -*- lexical-binding: t -*-
(require 'popup)
(require 'implicit-utils)

(defvar popup-border-chars '((horizontal . ?―)
                             (vertical . ?|)
                             (top-left . ?⌜)
                             (top-right . ?⌝)
                             (bottom-left . ?⌞)
                             (bottom-right . ?⌟)))

(defun +popup-menu--completing-read-default-lookup (choice list)
  choice)

(defun +popup--setup-text-face (string &rest props)
  "Strip the orginal text properties from STRING and apply supplied FACE."
  (let ((copy-string (concat string))
        (end (length string))
        (props (append props
                       `(:background ,(face-attribute 'popup-tip-face :background)))))
    (set-text-properties 0 end nil copy-string)
    (set-text-properties 0 end `(face ,props) copy-string)
    copy-string))

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


(defun +eldoc-display-in-popup (docs interactive)
  "Eldoc display function to display documentation in a popup."
  (let* ((body (mapconcat (lambda (doc)
                            (let* ((text (+popup--setup-text-face (car doc)
                                                                  :slant 'italic
                                                                  :foreground (doom-color 'fg)))
                                   (rest (cdr doc))
                                   (echo (plist-get rest :echo))
                                   (echo (when echo
                                           (+popup--setup-text-face echo)))
                                   (thing (plist-get rest :thing))
                                   (thing (when thing
                                            (+popup--setup-text-face (popup-x-to-string thing)
                                                                     :foreground (doom-color 'yellow)))))
                              (if echo
                                  echo
                                (when text
                                  (concat (when thing
                                            (concat thing ": "))
                                          text)))))
                          docs
                          "\n\n")))
    (when body
      (popup-tip body
                 :truncate nil
                 :margin 1
                 :around t
                 :nostrip t
                 :max-width (if interactive 100 60)
                 :max-height (if interactive 80 20)
                 :scroll-bar nil))))


(provide 'implicit-popup)
