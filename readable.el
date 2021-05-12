;;; readable.el --- Converts web pages into Org -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org-ml "5.6"))
;; Keywords: hypermedia outlines wp
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

;; Readable.el is a collection of convenient commands for working with
;; actual contents of web pages.
;;
;; It uses readability-cli (readable) to extract contents from web
;; pages.

;;; Code:

(require 'readable-document)

(defgroup readable nil
  "Converts web pages into Org."
  :group 'org)

;;;###autoload
(defun readable-insert-org-link (url)
  "Insert an Org link for URL."
  (interactive "sUrl: ")
  (derived-mode-p 'org-mode)
  (insert (org-link-make-string
           url (oref (readable-document-for-url url) title))))

;;;###autoload
(cl-defun readable-insert-org-toc (url &key include-header with-link)
  "Insert the table of contents of a web page into Org.

URL is the location of the document.

If INCLUDE-HEADER is non-nil, the output will contain headings
inside HTML header elements.

If WITH-LINK is non-nil, each item will be linked to the source
of the heading, if it has an id attribute."
  (interactive (list (read-string "Url: ")
                     :with-link current-prefix-arg))
  (derived-mode-p 'org-mode)
  (insert (mapconcat (lambda (x)
                       (let ((level (readable-toc-item-level x))
                             (text (readable-toc-item-text x))
                             (id (readable-toc-item-id x)))
                         (concat (make-string (* 2 (- level 2)) ?\s)
                                 "- "
                                 (if (and with-link id)
                                     (org-link-make-string (concat url "#" id) text)
                                   text))))
                     (-filter (if include-header
                                  #'identity
                                (-compose #'not #'readable-toc-item-in-header))
                              (readable-document-toc url))
                     "\n")
          "\n"))

;;;###autoload
(defun readable-read-in-shr (url)
  "Read URL in a non-file shr buffer."
  (interactive "sUrl: ")
  (let* ((document (readable-document-for-url url))
         (buffer-name (generate-new-buffer-name (format "*readable shr %s*"
                                                        (oref document title))))
         (out-buffer (generate-new-buffer buffer-name)))
    (with-current-buffer out-buffer
      (insert "<h1>" (oref document title) "</h1>\n"
              (oref document html-content))
      (shr-render-region (point-min) (point-max))
      (read-only-mode t)
      (goto-char (point-min)))
    (pop-to-buffer-same-window out-buffer)))

(provide 'readable)
;;; readable.el ends here
