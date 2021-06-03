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
(require 'dash)

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

(defcustom readable-fetch-log-file
  (expand-file-name "readable/fetch.log" user-emacs-directory)
  "Path to a file to which fetched URLs are recorded."
  :group 'readable
  :type 'file)

(eval-when-compile
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
    "Regexp pattern for URLs."))

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

(defun readable--html-cache-file (url)
  "Return the cache file name for URL in full path."
  (expand-file-name (concat (readable--file-escape-url-1 url)
                            ".html")
                    readable-cache-directory))

(defun readable--log-serialize (obj)
  (concat "- " (json-serialize obj) "\n"))

(defsubst readable--ts-format-iso-8601 (time)
  (format-time-string "%FT%X%:z" time))

(defun readable--log-url (url &rest args)
  (let ((obj (append (list :url url
                           :time (readable--format-iso-8601 (current-time)))
                     args)))
    (async-start
     `(lambda ()
        (with-temp-buffer
          (insert (readable--log-serialize ,obj))
          (append-to-file (point-min) (point-max) readable-fetch-log-file))))))

(defun readable-origin-source (url)
  "Return a file name that contains the original content of URL."
  (let ((cache-file (readable--html-cache-file url))
        (start-time (float-time)))
    (if (file-exists-p cache-file)
        (condition-case _
            (with-current-buffer (url-retrieve-synchronously
                                  url t t
                                  readable-download-timeout)
              (when url-http-end-of-headers
                (delete-region (point-min) url-http-end-of-headers))
              (unless (file-directory-p (file-name-directory cache-file))
                (make-directory (file-name-directory cache-file) t))
              (readable--log-url url
                                 :size (buffer-size)
                                 :duration (- (float-time) start-time))
              (write-file cache-file))
          (error (readable--log-url url :failed t
                                    :duration (- (float-time) start-time))))
      (readable--log-url url :cache t))
    cache-file))

;;;; Utilities

(defsubst readable--string-take (len string)
  "Take the first LEN characters of STRING."
  (if (> len (length string))
      string
    (substring string 0 len)))

(provide 'readable-fetch)
;;; readable-fetch.el ends here
