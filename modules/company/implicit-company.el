;;; -*- lexical-binding: t -*-


(defun +company-dabbrev-ignore-buffer-p (name)
  (or (string-match-p "\\`[ *]")
      (string-equal-ignore-case (file-name-extension name)
                                "csv")))

(provide 'implicit-company)
