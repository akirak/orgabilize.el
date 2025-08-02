;;; orgabilize-site.el --- Site-specific supports -*- lexical-binding: t -*-

;; Copyright (C) 2025 Akira Komamura

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

;; Site-specific backends for fetching web pages.

;;; Code:

(defun orgabilize-site-reddit-title (url)
  "Return the title of a Reddit page at URL."
  (when-let* ((thread-id (save-match-data
                           (when (string-match "/r/[^/]+/comments/\\([^/]+\\)/" url)
                             (match-string 1 url)))))
    (with-temp-buffer
      (url-insert-file-contents
       (format "https://www.reddit.com/comments/%s.json" thread-id))
      (goto-char (point-min))
      (thread-last
        (json-parse-buffer :object-type 'alist :array-type 'list)
        (nth 0)
        (alist-get 'data)
        (alist-get 'children)
        (nth 0)
        (alist-get 'data)
        (alist-get 'title)))))

(defun orgabilize-site-twitter-title (url)
  (when-let* ((html (orgabilize-site-twitter--oembed-html url)))
    (with-temp-buffer
      (insert html)
      (let (result)
        (goto-char (point-min))
        (while (< (point) (point-max))
          (cl-case (xmltok-forward)
            (data (push (buffer-substring-no-properties xmltok-start (point))
                        result))))
        (string-join (nreverse result))))))

(defun orgabilize-site-twitter--oembed-html (url)
  (when (string-match-p "/status/[0-9]+" url)
    (with-temp-buffer
      (url-insert-file-contents
       (format "https://publish.twitter.com/oembed?url=%s" (url-encode-url url)))
      (goto-char (point-min))
      (thread-last
        (json-parse-buffer :object-type 'alist :array-type 'list)
        (alist-get 'html)))))

(provide 'orgabilize-site)
;;; orgabilize-site.el ends here
