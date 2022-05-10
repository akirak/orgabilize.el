;;; orgabilize-org.el --- Conversion from Html to Org -*- lexical-binding: t -*-

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

;; This library provides a function for converting an html dom to Org.

;;; Code:

(require 'cl-lib)
(require 'org-ml)
(require 'orgabilize-document)

(defconst orgabilize-org-origin-url-property
  "ORGABILIZE_ORIGIN_URL")

(defconst orgabilize-org-src-language-keyword
  "ORGABILIZE_SRC_LANGUAGE")

(defcustom orgabilize-org-archive-directory
  (expand-file-name "orgabilize/" org-directory)
  "Directory in which Org outputs are saved."
  :group 'orgabilize
  :type 'directory)

(defcustom orgabilize-org-archive-filename-fn
  #'orgabilize-org-archive-filename-1
  ""
  :group 'orgabilize
  :type 'function)

(defcustom orgabilize-org-strictness nil
  ""
  :group 'orgabilize
  :type '(choice (const :tag "Strict" t)
                 (const :tag "Loose" nil)))

(cl-defstruct orgabilize-org-branch
  "Container for an Org branch.

This is used to prevent Org elements from flattening."
  element)

(cl-defstruct orgabilize-org-headline
  "Org heading."
  level id text)

(cl-defstruct orgabilize-org-archive-file title url tags)

(defvar orgabilize-org-archive-file-cache nil)

(defsubst orgabilize-org-wrap-branch (element)
  "Wrap ELEMENT in a `orgabilize-org-branch'."
  (make-orgabilize-org-branch :element element))

(cl-defgeneric orgabilize-org-unwrap (x)
  "Unwrap a surrouding structure of X, if any."
  (orgabilize-org-branch-element x))
(cl-defmethod orgabilize-org-unwrap ((x orgabilize-org-branch))
  "Unwrap a surrouding structure of X, if any."
  (orgabilize-org-branch-element x))
(cl-defmethod orgabilize-org-unwrap ((x string))
  "Unwrap a surrouding structure of X, if any."
  x)

(cl-defun orgabilize-org--parse-dom (dom &key src-language)
  "Transform DOM into a sequence representing part of an Org document.

The argument should be an HTML dom as parsed using
`libxml-parse-html-region'."
  ;; TODO Refactor and add unit tests for these functions.
  (cl-labels
      ((text-content
         (nodes)
         (->> nodes
              (--tree-map-nodes (pcase it
                                  ((and `(,_ . ,value)
                                        (guard (not (listp value))))
                                   t)
                                  (`(br . ,_)
                                   t))
                                (pcase it
                                  (`(br . ,_)
                                   "\n")
                                  (_
                                   it)))
              (--tree-reduce-from (if (and (stringp it)
                                           (not (equal "#" it)))
                                      (concat it acc)
                                    acc)
                                  "")))
       (normalize-space
         (str)
         (->> str
              (replace-regexp-in-string (rx (+ (any space))) " ")))
       (language-p
         (name)
         (when-let (mode (intern-soft (concat name "-mode")))
           (commandp mode)))
       (language-from-class
         (class)
         (when class
           (->> (split-string class "[[:space:]]+")
                (cl-remove-if #'string-empty-p)
                (--map (string-remove-prefix "language-" it))
                (--map (string-remove-prefix "lang-" it))
                (--filter (string-match-p "^[[:alnum:]]+$" it))
                (-filter #'language-p)
                (car))))
       (unwrap-inlines
         (children)
         (->> (-flatten children)
              (delq nil)
              (-map #'orgabilize-org-unwrap)))
       (go-inline-node
         (node)
         (pcase node
           (`(,tag ,attrs . ,children)
            (cl-case tag
              (a
               (if (equal children '("#"))
                   nil
                 (if-let (href (alist-get 'href attrs))
                     (->> (org-ml-build-link href)
                          (org-ml-set-children (unwrap-inlines (go-inline children)))
                          (orgabilize-org-wrap-branch))
                   (go-inline children))))
              (img
               ;; TODO: Download images using org-download
               (orgabilize-org-wrap-branch
                (org-ml-build-link (alist-get 'src attrs))))
              (i
               ;; i tag is often used without a content for
               ;; building an icon, so it is important to
               ;; check if it is not an empty element.
               ;;
               ;; Maybe we should print the class if it is an
               ;; empty element...
               (when-let (ochildren (go-inline children))
                 (->> (org-ml-build-italic)
                      (org-ml-set-children (unwrap-inlines ochildren))
                      (orgabilize-org-wrap-branch))))
              ((em cite)
               (->> (org-ml-build-italic)
                    (org-ml-set-children (unwrap-inlines (go-inline children)))
                    (orgabilize-org-wrap-branch)))
              ((strong b)
               (->> (org-ml-build-bold)
                    (org-ml-set-children (unwrap-inlines (go-inline children)))
                    (orgabilize-org-wrap-branch)))
              (sup
               (->> (org-ml-build-superscript)
                    (org-ml-set-children (unwrap-inlines (go-inline children)))
                    (orgabilize-org-wrap-branch)))
              (sub
               (->> (org-ml-build-subscript)
                    (org-ml-set-children (unwrap-inlines (go-inline children)))
                    (orgabilize-org-wrap-branch)))
              (code
               (orgabilize-org-wrap-branch
                (org-ml-build-code (text-content children))))
              (br
               "\n")
              ;; There are situations where a list element contains
              ;; a pre element. It is a valid HTML, but it is
              ;; impossible for an Org list to contain a source
              ;; block. For now, it is turned into an inline code.
              (pre
               (orgabilize-org-wrap-branch
                (org-ml-build-code (text-content children))))
              ;; Tags that are just ignored
              ((span time abbr figcaption small)
               (-flatten (mapcar #'go-inline children)))
              (svg
               (message "Warning: %s element is ignored"
                        tag)
               nil)
              (otherwise
               (pcase orgabilize-org-strictness
                 (`nil
                  (message "Unsupported tag %s in go-inline" tag)
                  (-flatten (mapcar #'go-inline children)))
                 (`t
                  (error "Unsupported tag %s in go-inline (with children %s)"
                         tag children))))))
           ((pred stringp)
            (normalize-space node))))
       (go-inline
         (nodes)
         (cond
          ((and nodes
                (listp nodes)
                (symbolp (car nodes)))
           (list (go-inline-node nodes)))
          ((stringp nodes)
           (list nodes))
          (t
           (-map #'go-inline-node nodes))))
       (non-nil-if-list
         (x)
         (if (listp x)
             (-non-nil x)
           x))
       (go-list
         (tag items)
         (let ((bullet (when (eq tag 'ol)
                         (make-symbol "bullet"))))
           (when bullet
             (set bullet 0))
           (->> (org-ml-build-plain-list)
                (org-ml-set-children
                 (->> items
                      (-map (lambda (child)
                              (pcase child
                                (`(li ,_ . ,content)
                                 content))))
                      (-non-nil)
                      (-map (lambda (item)
                              (go-item (if bullet
                                           (cl-incf (symbol-value bullet))
                                         '-)
                                       item))))))))
       (list-p
         (x)
         (and (listp x)
              (memq (car x) '(ul ol))))
       (whitespace-p
         (x)
         (and (stringp x)
              (string-match-p (rx bos (* space) eos) x)))
       (not-whitespace-p
         (x)
         (not (whitespace-p x)))
       (paragraph-p
         (x)
         (and (listp x)
              (eq (car x) 'paragraph)))
       (paragraph-or-plain-text-p
         (x)
         (or (stringp x)
             (paragraph-p x)))
       (clean-whitespace
         (elements)
         (when (and elements (whitespace-p (car elements)))
           (setq elements (cdr elements)))
         (when (and elements (whitespace-p (-last-item elements)))
           (setq elements (-butlast elements)))
         elements)
       (merge-paragraphs
         (elements)
         (->> (-group-by #'paragraph-or-plain-text-p (clean-whitespace elements))
              (-map #'cdr)
              (-map (lambda (xs)
                      (if (paragraph-or-plain-text-p (car xs))
                          (list (apply #'org-ml-build-paragraph
                                       (->> xs
                                            (--map (if (stringp it)
                                                       (list it)
                                                     (org-ml-get-children it)))
                                            (-flatten-n 1))))
                        xs)))
              (-flatten-n 1)))
       (go-item
         (bullet item)
         (-let* (((paragraph-content children) (-split-with (-not #'list-p) item))
                 (paragraph (when-let (oparagraph (unwrap-inlines
                                                   (go-inline paragraph-content)))
                              (->> (org-ml-build-paragraph)
                                   (org-ml-set-children oparagraph))))
                 (ochildren (->> children
                                 (-filter #'list-p)
                                 (--map (go-list (car it) (cddr it))))))
           (apply #'org-ml-build-item
                  :bullet bullet
                  (if paragraph
                      (cons paragraph ochildren)
                    ochildren))))
       ;; Handle both flow contents and block elements. This doesn't
       ;; strictly follow the model of HTML.
       (go
         (x)
         (pcase x
           (`(,tag ,attrs . ,children)
            (cl-case tag
              ((div main article section)
               (-map #'go children))
              ((h1 h2 h3 h4 h5 h6)
               (make-orgabilize-org-headline :level (string-to-number
                                                     (string-remove-prefix
                                                      "h" (symbol-name tag)))
                                             :id (alist-get 'id attrs)
                                             :text (normalize-space (text-content children))))
              (p
               (when-let (ochildren (unwrap-inlines
                                     (go-inline children)))
                 (condition-case err
                     (orgabilize-org-wrap-branch
                      (->> (org-ml-build-paragraph)
                           (org-ml-set-children ochildren)))
                   (error (error "Error while processing p:\n%s:\n%s"
                                 (string-join (--map (format "%s" it) ochildren)
                                              "\n")
                                 err)))))
              ((ul ol)
               (orgabilize-org-wrap-branch
                (go-list tag children)))
              (pre
               (pcase children
                 (`((code ,cattrs . ,content))
                  (orgabilize-org-wrap-branch
                   (org-ml-build-src-block
                    :language (or (language-from-class (alist-get 'class attrs))
                                  (language-from-class (alist-get 'class cattrs))
                                  src-language)
                    :value (text-content content))))
                 (_
                  (orgabilize-org-wrap-branch
                   (org-ml-build-src-block
                    :language (or (language-from-class (alist-get 'class attrs))
                                  src-language)
                    :value (text-content children))))))
              (blockquote
               (orgabilize-org-wrap-branch
                (->> (org-ml-build-quote-block)
                     (org-ml-set-children
                      (->> children
                           (-map #'go)
                           (-map #'orgabilize-org-unwrap)
                           (merge-paragraphs))))))
              (figure
               (-let* (((captions rest) (--separate (pcase it
                                                      (`(,tag . ,_)
                                                       (eq tag 'figcaption)))
                                                    children))
                       (caption-text (when captions
                                       (normalize-space (text-content captions))))
                       (ochildren (->> rest
                                       (--remove (and (stringp it)
                                                      (string-empty-p (string-trim it))))
                                       (-non-nil)
                                       (-map #'go))))
                 (if caption-text
                     (condition-case nil
                         (cons (orgabilize-org-wrap-branch
                                (->> (orgabilize-org-unwrap (car ochildren))
                                     (org-ml-set-caption! caption-text)))
                               (cdr ochildren))
                       (error (error "Error: %s" (orgabilize-org-unwrap (car ochildren)))))
                   ochildren)))
              (details
               (-let (((summaries rest) (--separate (pcase it
                                                      (`(,tag . ,_)
                                                       (eq tag 'summary)))
                                                    children)))
                 (orgabilize-org-wrap-branch
                  (->> (org-ml-build-special-block
                        "details"
                        :name (when summaries
                                (normalize-space (text-content (cddar summaries)))))
                       (org-ml-set-children (->> (-map #'go rest)
                                                 (-flatten)
                                                 ;; I want to remove this
                                                 (-remove #'stringp)
                                                 (-map #'orgabilize-org-unwrap)))))))
              (table
               (orgabilize-org-wrap-branch
                (org-ml-build-special-block
                 "A table is supposed to be here, but I haven't supported it yet.")))
              (header
               ;; Skip headers
               nil)
              ;; For inside a figure
              (img
               (orgabilize-org-wrap-branch
                (->> (org-ml-build-paragraph)
                     (org-ml-set-children (unwrap-inlines
                                           (go-inline (list x)))))))
              (otherwise
               (when-let (ochildren (unwrap-inlines
                                     (go-inline (list x))))
                 (orgabilize-org-wrap-branch
                  (->> (org-ml-build-paragraph)
                       (org-ml-set-children ochildren)))))))
           ((pred stringp)
            x)
           (_
            (error "Unsupported block content: %s" x)))))
    (->> dom
         ;; take the children of html
         (cddr)
         (-map #'cddr)
         (-flatten-n 1)
         (-map #'go)
         (-flatten)
         (-remove (lambda (s)
                    (and (stringp s)
                         (string-match-p (rx bos (+ (any space "\n")) eos) s)))))))

(cl-defstruct orgabilize-org-fragment
  "Alternative representation for content below a heading."
  head-content subheadlines)

(cl-defun orgabilize-org--to-fragment (branches &key url-without-fragment)
  "Transform BRANCHES into an `orgabilize-org-fragment'."
  (cl-labels
      ((set-post-blank
         (nodes)
         (--map (org-ml-set-property :post-blank 1 it) nodes))
       (set-subheadlines
         (nodes)
         (when nodes
           (pcase-let*
               ((`(,this . ,rest) nodes)
                (`(,descendants ,rest2) (--split-with (> (car it) (car this))
                                                      rest)))
             (cons (org-ml-headline-set-subheadlines
                    (set-subheadlines descendants)
                    (cdr this))
                   (set-subheadlines (copy-sequence rest2)))))))
    (let* ((partitions (-partition-before-pred #'orgabilize-org-headline-p branches))
           (head-content (when (and (car partitions)
                                    (not (orgabilize-org-headline-p (caar partitions))))
                           (-map #'orgabilize-org-unwrap (car partitions))))
           (subheadlines-with-lvs
            (->> (if head-content
                     (cdr partitions)
                   partitions)
                 (--map (let* ((h (car it))
                               (children (-map #'orgabilize-org-unwrap (cdr it)))
                               (drawer (when-let (id (orgabilize-org-headline-id h))
                                         (org-ml-build-property-drawer
                                          (org-ml-build-node-property
                                           "ORGABILIZE_ORIGIN_FRAGMENT_URL"
                                           (concat url-without-fragment "#" id))
                                          (org-ml-build-node-property
                                           "CUSTOM_ID"
                                           id)))))
                          (cons (orgabilize-org-headline-level h)
                                (->> (org-ml-build-headline!
                                      :level (orgabilize-org-headline-level h)
                                      :title-text (orgabilize-org-headline-text h)
                                      :post-blank 1
                                      :section-children
                                      (->> (if drawer
                                               (cons drawer children)
                                             children)
                                           (set-post-blank))))))))))
      (make-orgabilize-org-fragment
       :head-content (set-post-blank head-content)
       :subheadlines (set-subheadlines subheadlines-with-lvs)))))

(cl-defun orgabilize-org--build-headline (dom &key title level tags
                                              url-without-fragment
                                              src-language)
  "Build an Org headline from an html dom.

DOM must be an html dom. It constructs an Org headline with TITLE
at LEVEL, with optional TAGS."
  (declare (indent 1))
  (let ((fragment (-> (orgabilize-org--parse-dom
                       dom :src-language src-language)
                      (orgabilize-org--to-fragment
                       :url-without-fragment url-without-fragment))))
    (apply #'org-ml-build-headline!
           :level level
           :title-text title
           :tags tags
           :pre-blank 1
           :section-children
           (orgabilize-org-fragment-head-content fragment)
           (orgabilize-org-fragment-subheadlines fragment))))

(cl-defun orgabilize-org-archive-filename-1 (&key url title)
  "Return a file name without directory."
  (concat (if-let* ((url-obj (url-generic-parse-url url))
                    (host (url-host url-obj)))
              (concat host "-")
            "")
          (->> title
               (replace-regexp-in-string (rx (+ (any punct))) "")
               (replace-regexp-in-string (rx (+ space)) "-")
               (downcase))
          ".org"))

;;;###autoload
(defun orgabilize-org-archive (url)
  (interactive "sUrl: ")
  (unless (file-directory-p orgabilize-org-archive-directory)
    (make-directory orgabilize-org-archive-directory))
  (let* ((document (orgabilize-document-for-url url))
         (clean-url (oref document url))
         (title (oref document title))
         (outfile (expand-file-name (funcall orgabilize-org-archive-filename-fn
                                             :url clean-url
                                             :title title)
                                    orgabilize-org-archive-directory))
         (existing (or (find-buffer-visiting outfile)
                       (and (file-exists-p outfile)
                            (find-file-noselect outfile))))
         (dom (orgabilize-document-dom document))
         (level 1)
         orig-hash
         src-language
         new-buffer)
    (if existing
        (with-current-buffer existing
          (widen)
          (setq orig-hash (sha1 (current-buffer)))
          (save-excursion
            (goto-char (point-min))
            (when (org-before-first-heading-p)
              (let ((bound (save-excursion
                             (re-search-forward org-heading-regexp nil t)))
                    (case-fold-search t))
                (while (re-search-forward org-keyword-regexp bound t)
                  (let ((kwd (match-string 1)))
                    (when (equal kwd orgabilize-org-src-language-keyword)
                      (setq src-language
                            (string-trim (match-string 2)))))))))
          (if-let (start (org-find-property orgabilize-org-origin-url-property clean-url))
              (progn
                (goto-char start)
                (setq level (org-outline-level))
                (org-end-of-subtree)
                (delete-region start (point)))
            (re-search-forward org-heading-regexp nil t)))
      (with-current-buffer (setq new-buffer (create-file-buffer outfile))
        (insert "#+title: " title "\n")
        (when-let (excerpt (oref document excerpt))
          (org-ml-insert (point)
                         (org-ml-build-special-block
                          "excerpt"
                          (org-ml-build-paragraph! excerpt))))
        (setq buffer-file-name outfile)
        (org-mode)))
    (let ((buffer (or existing new-buffer)))
      (orgabilize-org--insert-fulltext :buffer buffer
                                       :dom dom
                                       :title title
                                       :level level
                                       :src-language src-language
                                       :url clean-url)
      (with-current-buffer buffer
        (if (or new-buffer
                (not (equal (sha1 (current-buffer))
                            orig-hash)))
            (save-buffer)
          (message "Not changed"))
        (pop-to-buffer-same-window (current-buffer))))))

(cl-defun orgabilize-org--insert-fulltext (&key buffer dom
                                                title level url
                                                src-language)
  "Insert a full text into the point."
  (with-current-buffer bufer
    (let ((start (point)))
      (thread-last
        (orgabilize-org--build-headline dom
          :title title
          :tags '("fulltext")
          :level level
          :url-without-fragment url
          :src-language (when (and src-language
                                   (not (string-empty-p src-language)))
                          src-language))
        (org-ml-headline-set-node-properties
         (list (org-ml-build-node-property
                orgabilize-org-origin-url-property clean-url)))
        (org-ml-insert (point)))
      (goto-char start)
      (orgabilize-org--set-visibility))))

(defun orgabilize-org--set-visibility ()
  (org-cycle '(16)))

;;;###autoload
(defun orgabilize-org-archive-from-file (file)
  "Convert a local html FILE to Org."
  (interactive "fFile: ")
  (let ((url (read-string "Url: ")))
    (orgabilize-save-file-as-url file url)
    (funcall-interactively #'orgabilize-org-archive url)))

;;;; Browsing the archive

;;;###autoload
(defun orgabilize-org-find-archived-file (file)
  "Find a file in the Org archive."
  (interactive (list (orgabilize-org-complete-file "File in Org archive: ")))
  (if-let (buffer (find-buffer-visiting file))
      (pop-to-buffer-same-window buffer)
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (orgabilize-org--find-fulltext)
        (org-cycle '(16)))
      (pop-to-buffer-same-window (current-buffer)))))

(defun orgabilize-org-complete-file (prompt)
  (completing-read prompt (orgabilize-org--file-completion)
                   nil t))

(defun orgabilize-org--file-completion ()
  (let ((files (thread-last
                 (orgabilize-org--archived-files)
                 (mapcar (lambda (path)
                           (put-text-property 0 (length path) 'invisible t path)
                           path)))))
    `(lambda (string pred action)
       (if (eq action 'metadata)
           '(metadata . ((category . orgabilize-org-archive-file)
                         (annotation-function . orgabilize-org-annotate-file)))
         (complete-with-action action ',files string pred)))))

(defun orgabilize-org--archived-files ()
  (unless orgabilize-org-archive-file-cache
    (setq orgabilize-org-archive-file-cache (make-hash-table :test #'equal)))
  (directory-files orgabilize-org-archive-directory t org-agenda-file-regexp))

(defun orgabilize-org--archive-file-info (file)
  (or (gethash file orgabilize-org-archive-file-cache)
      (when-let (buffer (find-buffer-visiting file))
        (with-current-buffer buffer
          (org-with-wide-buffer
           (goto-char (point-min))
           (puthash file (or (orgabilize-org--archive-info)
                             (error "No fulltext entry in %s" file))
                    orgabilize-org-archive-file-cache))))
      (with-temp-buffer
        (insert-file-contents file)
        (delay-mode-hooks (org-mode))
        (goto-char (point-min))
        (puthash file (or (orgabilize-org--archive-info)
                          (error "No fulltext entry in %s" file))
                 orgabilize-org-archive-file-cache))))

(defun orgabilize-org--archive-info ()
  (when (orgabilize-org--find-fulltext)
    (make-orgabilize-org-archive-file
     :title (substring-no-properties (org-get-heading t t t t))
     :tags (cl-remove "fulltext" (org-get-tags) :test #'equal)
     :url (org-entry-get nil "ORGABILIZE_ORIGIN_URL"))))

(defun orgabilize-org-annotate-file (file)
  (if-let (info (ignore-errors
                  (orgabilize-org--archive-file-info file)))
      (concat (orgabilize-org-archive-file-title info)
              " "
              (propertize (orgabilize-org-archive-file-url info)
                          'face 'font-lock-comment-face)
              " "
              (mapconcat (lambda (s)
                           (propertize s 'face 'org-tag))
                         (orgabilize-org-archive-file-tags info)
                         ":"))
    (propertize (format "%s: No fulltext entry"
                        (file-name-nondirectory
                         (substring-no-properties file)))
                'face 'font-lock-comment-face)))

(defun orgabilize-org--find-fulltext ()
  (catch 'orgabilize-fulltext
    (while (re-search-forward org-heading-regexp nil t)
      (when (member "fulltext" (org-get-tags))
        (throw 'orgabilize-fulltext t)))))

(provide 'orgabilize-org)
;;; orgabilize-org.el ends here
