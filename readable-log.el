;;; readable-log.el --- Logging -*- lexical-binding: t -*-

(require 'async)

(defun readable-log--serialize-plist (plist)
  "Serialize PLIST into a yaml list entry."
  (let ((entries (cl-loop for (key value) on plist by #'cddr
                          collect (format "%s: %s"
                                          (pcase (symbol-name key)
                                            ((and s (rx bol ":" (group (+ anything))))
                                             (match-string 1 s))
                                            (s s))
                                          (pcase value
                                            ('t "true")
                                            ('() "false")
                                            ((pred stringp) value)
                                            ((pred numberp) value)
                                            (_ (error "Did not match any of the supported types: %s"
                                                      value)))))))
    (concat "- " (string-join entries "\n  ") "\n")))

(defun readable-log-plist (plist file)
  "Log PLIST to a FILE."
  (async-start
   `(lambda ()
      (with-temp-buffer
        (insert (readable--log-serialize plist))
        (append-to-file (point-min) (point-max) file)))))

(provide readable-log)
;;; readable-log.el ends here
