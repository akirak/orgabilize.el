;;; orgabilize.el --- Converts web pages into Org -*- lexical-binding: t -*-

;; Copyright (C) 2021,2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org-ml "5.6") (dash "2.19"))
;; Keywords: hypermedia outlines wp
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

;; Orgabilize.el is a collection of convenient commands for working with
;; actual contents of web pages.
;;
;; It uses readability-cli (readable) to extract contents from web
;; pages.

;;; Code:

(require 'orgabilize-document)
(require 'dash)
(require 'ol)

(defgroup orgabilize nil
  "Converts web pages into Org."
  :group 'org)

;;;###autoload
(defun orgabilize-insert-org-link (url)
  "Insert an Org link for URL."
  (interactive "sUrl: ")
  (insert (org-link-make-string url (orgabilize-document-title url))))

;;;###autoload
(cl-defun orgabilize-insert-org-toc (url &key include-header with-link)
  "Insert the table of contents of a web page into Org.

URL is the location of the document.

If INCLUDE-HEADER is non-nil, the output will contain headings
inside HTML header elements.

If WITH-LINK is non-nil, each item will be linked to the source
of the heading, if it has an id attribute."
  (interactive (list (read-string "Url: ")
                     :with-link current-prefix-arg))
  (insert (mapconcat (lambda (x)
                       (let ((level (orgabilize-toc-item-level x))
                             (text (orgabilize-toc-item-text x))
                             (id (orgabilize-toc-item-id x)))
                         (concat (make-string (* 2 (- level 2)) ?\s)
                                 "- "
                                 (if (and with-link id)
                                     (org-link-make-string (concat url "#" id) text)
                                   text))))
                     (-filter (if include-header
                                  #'identity
                                (-compose #'not #'orgabilize-toc-item-in-header))
                              (orgabilize-document-toc url))
                     "\n")
          "\n"))

;;;###autoload
(defun orgabilize-view-source (url)
  (interactive "sUrl: ")
  (if-let (file (orgabilize-origin-source url))
      (find-file-read-only file)
    (user-error "There is no source for the url")))

;;;###autoload
(defun orgabilize-update-link-title ()
  "Update the title of the link at point.

You can also add this function to `org-ctrl-c-ctrl-c-hook'."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-match-data
      (when (thing-at-point-looking-at org-link-bracket-re)
        (let* ((m (match-data))
               (url (match-string 1))
               (title (when (and (not (match-string 2))
                                 (string-match-p (rx bol "http" (?  "s") ":") url))
                        (orgabilize-document-title url))))
          (when title
            (delete-region (nth 0 m) (nth 1 m))
            (insert (org-link-make-string url title))
            t))))))

(provide 'orgabilize)
;;; orgabilize.el ends here
