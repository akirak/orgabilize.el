;;; orgabilize-utils.el --- Utilities for orgabilize -*- lexical-binding: t -*-

(require 'url-parse)
(require 'xmltok)

(defvar xmltok-replacement)
(defvar xmltok-start)
(declare-function xmltok-forward "xmltok")

(defconst orgabilize-entities-json-url
  "https://html.spec.whatwg.org/entities.json")

(defvar orgabilize-xmltok-dtd nil)

(defun orgabilize--get-entities ()
  (let ((buffer (url-retrieve-synchronously orgabilize-entities-json-url))
        entries)
    (map-do (lambda (key value)
              (when (string-match (rx "&" (group (+ anything)) ";")
                                  key)
                (let ((string (gethash "characters" value)))
                  (push (cons (match-string 1 key)
                              (cons string string))
                        entries))))
            (unwind-protect
                (with-current-buffer buffer
                  (when (bound-and-true-p url-http-end-of-headers)
                    (delete-region (point-min)
                                   (if (markerp url-http-end-of-headers)
                                       (marker-position url-http-end-of-headers)
                                     url-http-end-of-headers)))
                  (goto-char (point-min))
                  (json-parse-buffer))
              (kill-buffer buffer)))
    (setq orgabilize-xmltok-dtd entries)))

(defsubst orgabilize--string-take (len string)
  "Take the first LEN characters of STRING."
  (if (> len (length string))
      string
    (substring string 0 len)))

(defun orgabilize-clean-url-string (string)
  "Return a url string with fragment stripped."
  (let ((obj (url-generic-parse-url string)))
    (setf (url-target obj) nil)
    (url-recreate-url obj)))

(defun orgabilize-decode-entity (string)
  "Decode SGML entities in STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward (rx "&#" (group (+ (any digit))) ";")
                                nil t)
        (replace-match (char-to-string (string-to-number (match-string 1))))))
    (buffer-string)))

(defun orgabilize--parse-sgml-text ()
  (let (tokens
        (xmltok-dtd (or orgabilize-xmltok-dtd
                        (orgabilize--get-entities))))
    (catch 'finish
      (while (< (point) (point-max))
        (cl-case (xmltok-forward)
          (end-tag
           (throw 'finish t))
          (entity-ref
           (push xmltok-replacement tokens))
          (data
           (push (buffer-substring-no-properties xmltok-start (point)) tokens)))))
    (string-join (nreverse tokens))))

(defun orgabilize-select-xml-nodes-by-tags (tags root)
  (declare (indent 1))
  (let (candidates)
    (cl-labels
        ((scan (node)
           (pcase node
             (`(,tag1 ,_attrs . ,children)
              (if (memq tag1 tags)
                  (push node candidates)
                (dolist (child children)
                  (scan child)))))))
      (scan root)
      (nreverse candidates))))

(provide 'orgabilize-utils)
;;; orgabilize-utils.el ends here
