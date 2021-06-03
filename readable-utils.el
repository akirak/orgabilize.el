;;; readable-utils.el --- Utilities -*- lexical-binding: t -*-

(defconst readable-url-regexp-for-escaping
  (rx bol (+ (any "+" alnum)) ":" (* (any "/"))
      ;; hostname
      (group (+ (not (any "/"))))
      ;; path
      (?  "/")
      (group (*? (not (any "?#"))))
      (?  "/")
      ;; query
      (?  (or (group "?" (* (not (any "#"))))
              (and "#" (+ anything))))
      eol)
  "Regexp pattern for URLs.")

(defun readable--file-escape-url-1 (url)
  "Return a path-safe string for URL."
  (save-match-data
    (if (string-match readable-url-regexp-for-escaping url)
        (concat (->> (match-string 1 url)
                  (replace-regexp-in-string (rx ".") "_"))
                "_"
                (->> (match-string 2 url)
                  (replace-regexp-in-string "/" "_")
                  (replace-regexp-in-string (rx (not (any "-_" alnum))) "")
                  (readable--string-take 128))
                "="
                (readable--string-take
                 10 (sha1 (concat (match-string 2 url)
                                  (match-string 3 url)))))
      (error "Did not match the URL pattern: %s" url))))

(defun readable--ts-format-iso-8601 (time)
  "Format TIME in the ISO-8601 date time format."
  (format-time-string "%FT%X%:z" time))

(defun readable--string-take (len string)
  "Take the first LEN characters of STRING."
  (if (> len (length string))
      string
    (substring string 0 len)))

(provide 'readable-utils)
;;; readable-utils.el ends here
