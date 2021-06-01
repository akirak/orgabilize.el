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
(require 'readable-transient)

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

;;;; Peek interface for working with a URL

(defvar readable-current-url nil)
(defvar readable-default-url nil)

(defcustom readable-browse-url-function #'browse-url
  "Function used in the browse action of `readable-peek'."
  :type 'function)

;;;###autoload
(transient-define-prefix readable-peek (url)
  "Run an action on URL."
  [:description
   (lambda () (format "Target: %s" readable-current-url))
   ("u" readable-transient-url-history)
   ("-" readable-transient-last-added-url)
   ("." readable-transient-url-at-point)
   ("0" readable-transient-default-url)]
  ;; ["Thing"
  ;;  ;; TODO: Define arguments
  ;;  (readable-peek:document)
  ;;  ("-d" "Document or URL" "document")
  ;;  ("-h" "Section (as a quote)" "section")
  ;;  ("-q" "Paragraph (as a quote)" "quote")
  ;;  ("-s" "Source block" "src")]
  ["Org actions"
   :if readable--org-mode-p
   ("i" "Insert to Org" "org" readable-peek--insert-org-action)]
  ;; ["Actions"
  ;;  [("r" "Read or archive" readable-peek--read-action)
  ;;   ("C-w" "Copy thing" readable-peek--copy-action)
  ;;   ("C-b" "Browse in external browser" readable-peek--browse-action)]]
  (interactive (list (or readable-current-url
                         (or (readable--url-at-point)
                             (readable--last-url)
                             (read-string "Url: ")))))
  (setq readable-current-url url
        readable-default-url url)
  (transient-setup 'readable-peek))

(transient-define-argument readable-peek:document ()
  :description "Document or URL"
  :class 'transient-option
  :shortarg "-m"
  :argument "--message=")

(defun readable-peek-arguments ()
  (transient-args 'readable-peek))

(defun readable--org-mode-p ()
  (derived-mode-p 'org-mode))

(defun readable-peek--insert-org-action (url args)
  (interactive (list readable-current-url
                     (transient-args 'readable-peek)))
  (message "%s: %s" url args x))

(defun readable-peek--read-action (url args)
  (interactive (list readable-current-url
                     (transient-args 'readable-peek)))
  (message "%s: %s" url args x))

(defun readable-peek--copy-action (url args)
  (interactive (list readable-current-url
                     (transient-args 'readable-peek)))
  (message "%s: %s" url args x))

(defun readable-peek--browse-action (url args)
  (interactive (list readable-current-url
                     (transient-args 'readable-peek)))
  (message "%s: %s" url args x))

;; (readable-peek "https://reddit.com")

(provide 'readable)
;;; readable.el ends here
