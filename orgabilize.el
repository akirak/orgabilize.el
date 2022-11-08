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

(declare-function thing-at-point-looking-at "thingatpt")

(defgroup orgabilize nil
  "Converts web pages into Org."
  :prefix "orgabilize-"
  :group 'org)

(defcustom orgabilize-use-canonical-urls t
  "Whether to use canonical URLs to link to resources."
  :group 'orgabilize
  :type 'boolean)

;;;###autoload
(defun orgabilize-insert-org-link (url &optional fragment)
  "Insert an Org link for URL."
  (interactive (list (read-string "Url: ")
                     current-prefix-arg))
  (insert (orgabilize-make-link-string url fragment)))

(defun orgabilize-make-link-string (url &optional fragment)
  "Return an Org link string for URL."
  (let ((clean-url (orgabilize--url-for-link url)))
    (if fragment
        (if-let (fragment (orgabilize-document-fragment url))
            (org-link-make-string (concat (orgabilize--url-for-link url)
                                          "#" fragment)
                                  (or (orgabilize-document-fragment-title
                                       url fragment)
                                      (orgabilize-document-title url)))
          (org-link-make-string (orgabilize--url-for-link url)
                                (orgabilize-document-title url)))
      (org-link-make-string (orgabilize--url-for-link url)
                            (orgabilize-document-title url)))))

;;;###autoload
(cl-defun orgabilize-insert-org-toc (url &key include-header with-link
                                         depth checkbox)
  "Insert the table of contents of a web page into Org.

URL is the location of the document.

If \[universal-prefix] is given in an interactive usage, this
function prompts for options.

If INCLUDE-HEADER is non-nil, the output will contain headings
inside HTML header elements.

If WITH-LINK is non-nil, each item will be linked to the source
of the heading, if it has an id attribute.

If DEPTH is a positive integer, it limits the maximum level of
items fo the value.

If CHECKBOX is non-nil, add an empty checkbox to each item."
  (interactive (let ((url (read-string "Url: ")))
                 (if current-prefix-arg
                     (cons url (orgabilize--read-toc-options))
                   (list url))))
  (let ((url (orgabilize--url-for-link url)))
    (insert (mapconcat (lambda (x)
                         (let ((level (orgabilize-toc-item-level x))
                               (text (orgabilize-toc-item-text x))
                               (id (orgabilize-toc-item-id x)))
                           (concat (make-string (* 2 (- level 2)) ?\s)
                                   "- "
                                   (if checkbox
                                       "[ ] "
                                     "")
                                   (if (and with-link id)
                                       (org-link-make-string (concat url "#" id) text)
                                     text))))
                       (thread-last
                         (orgabilize-document-toc url)
                         (--filter (if include-header
                                       t
                                     (not (orgabilize-toc-item-in-header it))))
                         (--filter (if (and (numberp depth) (> depth 0))
                                       (<= (orgabilize-toc-item-level it) (1+ depth))
                                     t)))
                       "\n")
            "\n")))

(defun orgabilize--read-toc-options ()
  "Return a plist for TOC options."
  (list :include-header (yes-or-no-p "Include items in the header? ")
        :with-link (yes-or-no-p "With link? ")
        :depth (read-number "Depth (0 to unlimited): ")
        :checkbox (yes-or-no-p "Add checkboxes? ")))

;;;###autoload
(defun orgabilize-insert-org-toc-for-headline ()
  "Insert the TOC for the URL of the headline."
  (interactive)
  (unless (and (derived-mode-p 'org-mode)
               (not (org-before-first-heading-p)))
    (user-error "Not in org-mode or before the first headline"))
  (let ((headline (org-get-heading t t t t)))
    (save-match-data
      (if (and (string-match org-link-bracket-re headline)
               (string-match-p (rx bol "http" (?  "s") ":")
                               (match-string 1 headline)))
          (apply #'orgabilize-insert-org-toc
                 (match-string 1 headline)
                 (when current-prefix-arg
                   (orgabilize--read-toc-options)))
        (user-error "The headline is not a link or not an HTTP(S) url")))))

;;;###autoload
(defun orgabilize-view-source (url)
  (interactive "sUrl: ")
  (let ((buffer-name (format "*Orgabilize<%s>*" url)))
    (if (get-buffer buffer-name)
        (pop-to-buffer buffer-name)
      (if-let (buffer (orgabilize-content-buffer url))
          (with-current-buffer buffer
            (html-mode)
            (rename-buffer buffer-name)
            (pop-to-buffer (current-buffer)))
        (user-error "There is no source for the url")))))

;;;###autoload
(defun orgabilize-update-link-title ()
  "Update the title of the link at point.

You can also add this function to `org-ctrl-c-ctrl-c-hook'."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-match-data
      (cond
       ((thing-at-point-looking-at org-link-bracket-re)
        (let* ((m (match-data))
               (url (match-string 1))
               (title (when (and (not (match-string 2))
                                 (string-match-p (rx bol "http" (?  "s") ":") url))
                        (orgabilize-document-title url))))
          (when title
            (delete-region (nth 0 m) (nth 1 m))
            (insert (org-link-make-string url title))
            t)))
       ((thing-at-point-looking-at org-link-plain-re)
        (let* ((m (match-data))
               (url (match-string 0))
               (title (orgabilize-document-title url)))
          (when title
            (delete-region (nth 0 m) (nth 1 m))
            (insert (org-link-make-string url title))
            t)))))))

(defun orgabilize--url-for-link (url)
  "Return a URL for linking."
  (or (and orgabilize-use-canonical-urls
           (orgabilize-document-canonical-url url))
      (orgabilize-clean-url-string url)))

(provide 'orgabilize)
;;; orgabilize.el ends here
