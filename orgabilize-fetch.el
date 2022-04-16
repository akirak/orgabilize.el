;;; orgabilize-fetch.el --- Downloading facility -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/orgabilize.el

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
(require 'url-parse)
(require 'subr-x)

;; Silence byte compiler
(defvar url-http-end-of-headers)

(defcustom orgabilize-cache-directory
  (expand-file-name "orgabilize/cache" user-emacs-directory)
  "Directory in which content cache are stored."
  :group 'readable
  :type 'directory)

(defcustom orgabilize-download-timeout 3
  "Timeout of HTTP requests in seconds."
  :group 'readable
  :type 'number)

(defun orgabilize--file-escape-url (url)
  "Return a path-safe string for URL."
  (let* ((obj (url-generic-parse-url url))
         (filename (url-filename obj))
         (path (save-match-data
                 (if (string-match (rx bol (+ (not (any "?")))) filename)
                     (match-string 0 filename)
                   filename))))
    (concat (url-host obj)
            (thread-last
              path
              (string-remove-suffix "/")
              (replace-regexp-in-string "/" "_")
              (replace-regexp-in-string (rx (not (any "-_" alnum))) "")
              (orgabilize--string-take 128))
            "__"
            (orgabilize--string-take 10 (sha1 filename)))))

(defun orgabilize--html-cache-file (url)
  "Return the cache file name for URL in full path."
  (expand-file-name (concat (orgabilize--file-escape-url url)
                            ".html")
                    orgabilize-cache-directory))

(defun orgabilize-origin-source (url)
  "Return a file name that contains the original content of URL."
  (let ((cache-file (orgabilize--html-cache-file url)))
    (unless (file-exists-p cache-file)
      (with-current-buffer (url-retrieve-synchronously
                            url t t
                            orgabilize-download-timeout)
        (when url-http-end-of-headers
          (delete-region (point-min) url-http-end-of-headers))
        (write-file cache-file)))
    cache-file))

;;;; Utilities

(defsubst orgabilize--string-take (len string)
  "Take the first LEN characters of STRING."
  (if (> len (length string))
      string
    (substring string 0 len)))

(provide 'orgabilize-fetch)
;;; orgabilize-fetch.el ends here
