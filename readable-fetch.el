;;; readable-fetch.el --- Downloading facility -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/readable.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides facilities for downloading web pages.

;;; Code:

(require 'url-http)
(require 'subr-x)

;; Silence byte compiler
(defvar url-http-end-of-headers)

(defcustom readable-cache-directory
  (expand-file-name "readable/cache" user-emacs-directory)
  "Directory in which content cache are stored."
  :group 'readable
  :type 'directory)

(defcustom readable-download-timeout 3
  "Timeout of HTTP requests in seconds."
  :group 'readable
  :type 'number)

(defconst readable-url-regexp-for-escaping
  (rx bol (+ (any "+" alnum)) ":" (* (any "/"))
      ;; hostname
      (group (+ (not (any "/"))))
      ;; path
      (group (* (not (any "?#"))))
      ;; query
      (?  (group "?" (* (not (any "#"))))))
  "Regexp pattern for URLs.")

(defun readable--file-escape-url (url)
  "Return a path-safe string for URL."
  (save-match-data
    (if (string-match readable-url-regexp-for-escaping url)
        (concat (->> (match-string 1 url)
                  (replace-regexp-in-string (rx ".") "_"))
                "_"
                (->> (match-string 2 url)
                  (string-remove-prefix "/")
                  (string-remove-suffix "/")
                  (replace-regexp-in-string "/" "_")
                  (replace-regexp-in-string (rx (not (any "-_" alnum))) "")
                  (readable--string-take 128))
                "__"
                (readable--string-take
                 10 (sha1 (match-string 0 url))))
      (error "Did not match the URL pattern: %s" url))))

(defun readable--html-cache-file (url)
  "Return the cache file name for URL in full path."
  (expand-file-name (concat (readable--file-escape-url url)
                            ".html")
                    readable-cache-directory))

(defun readable-origin-source (url)
  "Return a file name that contains the original content of URL."
  (let ((cache-file (readable--html-cache-file url)))
    (unless (file-exists-p cache-file)
      (with-current-buffer (url-retrieve-synchronously
                            url t t
                            readable-download-timeout)
        (when url-http-end-of-headers
          (delete-region (point-min) url-http-end-of-headers))
        (write-file cache-file)))
    cache-file))

;;;; Utilities

(defsubst readable--string-take (len string)
  "Take the first LEN characters of STRING."
  (if (> len (length string))
      string
    (substring string 0 len)))

(provide 'readable-fetch)
;;; readable-fetch.el ends here
