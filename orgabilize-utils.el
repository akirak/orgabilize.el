;;; orgabilize-utils.el --- Utilities for orgabilize -*- lexical-binding: t -*-

(require 'url-parse)

(defun orgabilize-clean-url-string (string)
  "Return a url string with fragment stripped."
  (let ((obj (url-generic-parse-url string)))
    (setf (url-target obj) nil)
    (url-recreate-url obj)))

(provide 'orgabilize-utils)
;;; orgabilize-utils.el ends here
