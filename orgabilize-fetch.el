;;; orgabilize-fetch.el --- Downloading facility -*- lexical-binding: t -*-

;; Copyright (C) 2021,2022 Akira Komamura

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
(require 'dash)
(require 'orgabilize-utils)
(require 'org)

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

(defcustom orgabilize-fetch-log-file
  (expand-file-name "orgabilize/fetch.log" user-emacs-directory)
  "File to which fetched URLs are logged."
  :group 'readable
  :type 'file)

(defun orgabilize--file-escape-url (url)
  "Return a path-safe string for URL."
  (let* ((obj (url-generic-parse-url url))
         (filename (url-filename obj))
         (path (save-match-data
                 (if (string-match (rx bol (+ (not (any "?")))) filename)
                     (match-string 0 filename)
                   filename))))
    (concat (url-host obj)
            (->> path
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

(defun orgabilize-content-buffer (url)
  "Return a buffer that contains the source of URL.

The buffer is created every time you create this function, so you
have to clean it up for yourself.

The returned buffer will be in `fundamental-mode'.

If the retrieval fails due to timeout or other kind of errors,
nil is returned."
  (catch 'fetched
    (let ((cache-file (orgabilize--html-cache-file url))
          (coding-system-for-read 'utf-8))
      (if (file-exists-p cache-file)
          (with-current-buffer (generate-new-buffer "*Orgabilize Src*")
            (insert-file-contents cache-file)
            (current-buffer))
        (let ((buffer (url-retrieve-synchronously
                       url t t
                       orgabilize-download-timeout)))
          (unless (buffer-live-p buffer)
            (throw 'fetched nil))
          (with-current-buffer buffer
            (when (bound-and-true-p url-http-end-of-headers)
              (delete-region (point-min)
                             (if (markerp url-http-end-of-headers)
                                 (marker-position url-http-end-of-headers)
                               url-http-end-of-headers)))
            ;; Trim preceding spaces (including newlines).
            (goto-char (point-min))
            (save-match-data
              (when (looking-at (rx (+ space)))
                (delete-region (point-min) (nth 1 (match-data)))))

            ;; Defer logging.
            (let ((log-msg (concat (format-time-string (org-time-stamp-format t t)
                                                       (current-time))
                                   " " url "\n")))
              (run-with-idle-timer
               1 nil
               `(lambda ()
                  (with-temp-buffer
                    (insert ,log-msg)
                    (write-region (point-min) (point-max) orgabilize-fetch-log-file t)
                    (message nil)))))

            ;; If the content is empty, return nil.
            (when (= (buffer-size) 0)
              (throw 'fetched nil))

            (write-region (point-min) (point-max) cache-file 'silent)

            buffer))))))

(defmacro orgabilize-with-source-as-buffer (url &rest progn)
  "Evaluate an expression with a buffer for the content of a url."
  (declare (indent 1))
  `(when-let (buffer (ignore-errors
                       (orgabilize-content-buffer ,url)))
     (unwind-protect
         (with-current-buffer buffer
           ,@progn)
       (kill-buffer buffer))))

(defun orgabilize-save-file-as-url (file &optional url)
  (interactive "fFile: ")
  (let* ((url (or url (read-string "Url: ")))
         (outfile (orgabilize--html-cache-file url)))
    (copy-file file outfile t)))

(provide 'orgabilize-fetch)
;;; orgabilize-fetch.el ends here
