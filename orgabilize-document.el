;;; orgabilize-document.el --- Parsing HTML -*- lexical-binding: t -*-

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

;; This library provides a class for working with web pages.

;;; Code:

(require 'orgabilize-fetch)
(require 'eieio)
(require 'eieio-base)

;;;; Custom variables

(defcustom orgabilize-executable "readable"
  "Path to the executable of readability-cli."
  :group 'readable
  :type 'file)

(defcustom orgabilize-args nil
  "List of command line arguments passed to readability-cli."
  :group 'readable
  :type '(repeat string))

;;;; Variables
(defvar orgabilize-document-tracker nil)

;;;; Running readable

(defun orgabilize--json-data (url &optional source-file)
  "Return a buffer for the output of readability-cli.

URL is the location of the document.

Optionally, you can specify SOURCE-FILE which contains the
original content body of the url. This is intended for testing."
  (with-temp-buffer
    (unless (zerop (apply #'call-process
                          orgabilize-executable
                          nil (list (current-buffer) nil) nil
                          "--json" "-b" url
                          (append orgabilize-args
                                  (list (or source-file
                                            (orgabilize-origin-source url))))))
      (error "Readable failed with non-zero exit code"))
    (goto-char (point-min))
    (json-parse-buffer :object-type 'alist
                       :null-object nil)))

;;;; orgabilize-document class

(defclass orgabilize-document (eieio-instance-tracker)
  ((tracking-symbol :initform 'orgabilize-document-tracker)
   (url :initarg :url
        :type string)
   (buffer-creation-time :initarg :buffer-creation-time
                         :type list)
   (title :initarg :title
          :type string)
   (excerpt :initarg :excerpt
            :initform nil
            :type (or string null))
   (byline :initarg :byline
           :type (or string null))
   (html-content :initarg :html-content
                 :type string)
   ;; Lazy fields that are not always available
   (toc :initarg :toc
        :initform nil
        :type (or list null))))

(cl-defun orgabilize-document-for-url (url &key source-file)
  "Return an instance of `orgabilize-document'.

It constructs an instance of the class for URL.

You can optionally specify SOURCE-FILE for retrieving the content
from the file. This is intended for testing."
  (or (eieio-instance-tracker-find url 'url 'orgabilize-document-tracker)
      (let ((data (orgabilize--json-data url source-file)))
        (make-instance 'orgabilize-document
                       :url url
                       :buffer-creation-time (current-time)
                       :title (alist-get 'title data)
                       :excerpt (alist-get 'excerpt data)
                       :byline (alist-get 'byline data)
                       :html-content (alist-get 'html-content data)))))

(cl-defgeneric orgabilize-document-dom (x)
  "Return the html dom of the content of X.")
(cl-defmethod orgabilize-document-dom ((url string))
  "Return the html dom of the content of URL."
  (orgabilize-document-dom (orgabilize-document-for-url url)))
(cl-defmethod orgabilize-document-dom ((x orgabilize-document))
  "Return the html dom of the content of X."
  (with-temp-buffer
    (insert (oref x html-content))
    (libxml-parse-html-region (point-min) (point-max)
                              (oref x url))))

(cl-defgeneric orgabilize-document-toc (x)
  "Return the table of contents of X.")
(cl-defmethod orgabilize-document-toc ((url string))
  "Return the table of contents of URL."
  (orgabilize-document-toc (orgabilize-document-for-url url)))
(cl-defmethod orgabilize-document-toc ((x orgabilize-document))
  "Return the table of contents of X."
  (or (oref x toc)
      (oset x toc (orgabilize--dom-toc (orgabilize-document-dom x)))))

;;;; Extracting toc

(cl-defstruct orgabilize-toc-item
  "Heading in a table of contents."
  level text id in-header)

(defun orgabilize--dom-toc (dom)
  "Extract the table of contents from DOM."
  (let (items)
    (cl-labels
        ((go (in-header list)
           (pcase list
             (`(,tag ,attrs . ,children)
              (if-let (level (cl-case tag
                               (h2 2)
                               (h3 3)
                               (h4 4)
                               (h5 5)
                               (h6 6)))
                  (push (make-orgabilize-toc-item
                         :level level
                         :text (string-trim (parse-heading-inlines children))
                         :id (alist-get 'id attrs)
                         :in-header in-header)
                        items)
                (when (sequencep children)
                  (dolist (child children)
                    (go (or (eq tag 'header) in-header) child)))))))
         (parse-heading-inlines (nodes)
           (-> (-map #'parse-heading-inline nodes)
               (-non-nil)
               (string-join "")))
         (parse-heading-inline (node)
           (pcase node
             ("#" "")
             ((pred stringp) node)
             (`(,_tag ,_attrs . ,children)
              (parse-heading-inlines children)))))
      (go nil dom))
    (nreverse items)))

(provide 'orgabilize-document)
;;; orgabilize-document.el ends here
