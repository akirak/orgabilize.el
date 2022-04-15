;;; readable-org.el --- Conversion from Html to Org -*- lexical-binding: t -*-

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

;; This library provides a function for converting an html dom to Org.

;;; Code:

(require 'cl-lib)
(require 'org-ml)
(require 'readable-document)

(cl-defstruct readable-org-branch
  "Container for an Org branch.

This is used to prevent Org elements from flattening."
  element)

(cl-defstruct readable-org-headline
  "Org heading."
  level id text)

(defsubst readable-org-wrap-branch (element)
  "Wrap ELEMENT in a `readable-org-branch'."
  (make-readable-org-branch :element element))

(cl-defgeneric readable-org-unwrap (x)
  "Unwrap a surrouding structure of X, if any."
  (readable-org-branch-element x))
(cl-defmethod readable-org-unwrap ((x readable-org-branch))
  "Unwrap a surrouding structure of X, if any."
  (readable-org-branch-element x))
(cl-defmethod readable-org-unwrap ((x string))
  "Unwrap a surrouding structure of X, if any."
  x)

(defun readable-org--parse-dom (dom)
  "Transform DOM into a sequence representing part of an Org document.

The argument should be an HTML dom as parsed using
`libxml-parse-html-region'."
  (cl-labels
      ((text-content
         (nodes)
         (->> nodes
              (--tree-map-nodes (pcase it
                                  ((and `(,_ . ,value)
                                        (guard (not (listp value))))
                                   t))
                                nil)
              (--tree-reduce-from (if (and (stringp it)
                                           ;; Sharp is often used as an
                                           ;; anchor, which should be
                                           ;; stripped from the output.
                                           (not (string-equal it "#")))
                                      (concat it acc)
                                    acc)
                                  "")))
       (normalize-space
         (str)
         (->> str
              (replace-regexp-in-string (rx (+ (any space "\n"))) " ")
              (string-trim)))
       (language-p
         (name)
         (when-let (mode (intern-soft (concat name "-mode")))
           (commandp mode)))
       (language-from-class
         (class)
         (when class
           (->> (split-string class "\n")
                (-map #'string-trim)
                (--filter (string-match-p "[[:alnum:]]" it))
                (-filter #'language-p)
                (car))))
       (go-inline
         (nodes)
         (->> nodes
              (-map (lambda (node)
                      (pcase node
                        (`(,tag ,attrs . ,children)
                         (cl-case tag
                           (a
                            (if (equal children '("#"))
                                nil
                              (if-let (href (alist-get 'href attrs))
                                  (->> (org-ml-build-link href)
                                       (org-ml-set-children (go-inline children)))
                                (go-inline children))))
                           (img
                            ;; TODO: Download images using org-download
                            (org-ml-build-link (alist-get 'src attrs)))
                           (i
                            ;; i tag is often used without a content for
                            ;; building an icon, so it is important to
                            ;; check if it is not an empty element.
                            ;;
                            ;; Maybe we should print the class if it is an
                            ;; empty element...
                            (when-let (ochildren (go-inline children))
                              (->> (org-ml-build-italic)
                                   (org-ml-set-children ochildren))))
                           ((em cite)
                            (->> (org-ml-build-italic)
                                 (org-ml-set-children (go-inline children))))
                           ((strong b)
                            (->> (org-ml-build-bold)
                                 (org-ml-set-children (go-inline children))))
                           (sup
                            (->> (org-ml-build-superscript)
                                 (org-ml-set-children (go-inline children))))
                           (sub
                            (->> (org-ml-build-subscript)
                                 (org-ml-set-children (go-inline children))))
                           (code
                            (org-ml-build-code (text-content children)))
                           (br
                            "\n")
                           ;; There are situations where a list element contains
                           ;; a pre element. It is a valid HTML, but it is
                           ;; impossible for an Org list to contain a source
                           ;; block. For now, it is turned into an inline code.
                           (pre
                            (org-ml-build-code (text-content children)))
                           ;; Tags that are just ignored
                           ((span time abbr figcaption)
                            (go-inline children))
                           (otherwise
                            (error "Unsupported tag %s in go-inline (with children %s)"
                                   tag children))))
                        ((pred stringp)
                         (normalize-space node)))))
              ;; Unwrap lists that are not org elements
              (-map (lambda (tree)
                      (--tree-map-nodes (and (listp it)
                                             (-all-p #'stringp it))
                                        (apply #'concat it)
                                        tree)))
              ;; You can't flatten org-ml structs!
              (-non-nil)))
       (non-nil-if-list
         (x)
         (if (listp x)
             (-non-nil x)
           x))
       (inline-leaf
         (nodes)
         (vector 'inline (go-inline nodes)))
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
       (go-item
         (bullet item)
         (-let* (((paragraph-content children) (-split-with (-not #'list-p) item))
                 (paragraph (when-let (oparagraph (go-inline paragraph-content))
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
              ((div main article)
               (-map #'go children))
              ((h1 h2 h3 h4 h5 h6)
               (make-readable-org-headline :level (string-to-number
                                                   (string-remove-prefix
                                                    "h" (symbol-name tag)))
                                           :id (alist-get 'id attrs)
                                           :text (normalize-space (text-content children))))
              (p
               (when-let (ochildren (go-inline children))
                 (condition-case nil
                     (readable-org-wrap-branch
                      (->> (org-ml-build-paragraph)
                           (org-ml-set-children ochildren)))
                   (error (error "Error: %s" ochildren)))))
              ((ul ol)
               (readable-org-wrap-branch
                (go-list tag children)))
              (pre
               (pcase children
                 (`((code ,_ . ,content))
                  (readable-org-wrap-branch
                   (org-ml-build-src-block
                    :value (text-content content))))
                 (_
                  (readable-org-wrap-branch
                   (org-ml-build-src-block
                    :value (text-content children))))))
              (blockquote
               (readable-org-wrap-branch
                (->> (org-ml-build-quote-block)
                     (org-ml-set-children (-map (-compose #'readable-org-unwrap
                                                          #'go)
                                                children)))))
              (figure
               (-let* (((captions rest) (--separate (pcase it
                                                      (`(,tag . ,_)
                                                       (eq tag 'figcaption)))
                                                    children))
                       (caption-text (when captions
                                       (normalize-space (text-content captions))))
                       (ochildren (->> rest
                                       (--remove (and (stringp it) (string-empty-p (trim-string it))))
                                       (-non-nil)
                                       (-map #'go))))
                 (if caption-text
                     (condition-case nil
                         (cons (readable-org-wrap-branch
                                (->> (readable-org-unwrap (car ochildren))
                                     (org-ml-set-caption! caption-text)))
                               (cdr ochildren))
                       (error (error "Error: %s" (readable-org-unwrap (car ochildren)))))
                   ochildren)))
              (details
               (-let (((summaries rest) (--separate (pcase it
                                                      (`(,tag . ,_)
                                                       (eq tag 'summary)))
                                                    children)))
                 (readable-org-wrap-branch
                  (->> (org-ml-build-special-block
                        "details"
                        :name (when summaries
                                (normalize-space (text-content (cddar summaries)))))
                       (org-ml-set-children (->> (-map #'go rest)
                                                 (-flatten)
                                                 ;; I want to remove this
                                                 (-remove #'stringp)
                                                 (-map #'readable-org-unwrap)))))))
              (table
               (readable-org-wrap-branch
                (org-ml-build-special-block
                 "A table is supposed to be here, but I haven't supported it yet.")))
              (header
               ;; Skip headers
               nil)
              ;; For inside a figure
              (img
               (readable-org-wrap-branch
                (->> (org-ml-build-paragraph)
                     (org-ml-set-children (go-inline (list x))))))
              (otherwise
               (when-let (ochildren (go-inline (list x)))
                 (readable-org-wrap-branch
                  (->> (org-ml-build-paragraph)
                       (org-ml-set-children ochildren)))))))
           ((pred stringp)
            x))))
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

(cl-defstruct readable-org-fragment
  "Alternative representation for content below a heading."
  head-content subheadlines)

(defun readable-org--to-fragment (branches)
  "Transform BRANCHES into an `readable-org-fragment'."
  (let* ((partitions (-partition-before-pred #'readable-org-headline-p branches))
         (head-content (when (and (car partitions)
                                  (not (readable-org-headline-p (caar partitions))))
                         (-map #'readable-org-unwrap (car partitions)))))
    (cl-flet
        ((set-post-blank
          (nodes)
          (--map (org-ml-set-property :post-blank 1 it) nodes)))
      (make-readable-org-fragment
       :head-content (set-post-blank head-content)
       :subheadlines
       (->> (if head-content
                (cdr partitions)
              partitions)
         (--map (let* ((h (car it))
                       (children (-map #'readable-org-unwrap (cdr it)))
                       (drawer (when-let (id (readable-org-headline-id h))
                                 (->> (org-ml-build-node-property "CUSTOM_ID" id)
                                   (org-ml-build-property-drawer)))))
                  (->> (org-ml-build-headline!
                        :level (readable-org-headline-level h)
                        :title-text (readable-org-headline-text h)
                        :post-blank 1
                        :section-children
                        (->> (if drawer
                                 (cons drawer children)
                               children)
                          (set-post-blank)))))))))))

(cl-defun readable-org--build-headline (dom &key title level tags)
  "Build an Org headline from an html dom.

DOM must be an html dom. It constructs an Org headline with TITLE
at LEVEL, with optional TAGS."
  (declare (indent 1))
  (let ((fragment (->> (readable-org--parse-dom dom)
                    (readable-org--to-fragment))))
    (apply #'org-ml-build-headline!
           :level level
           :title-text title
           :tags tags
           :pre-blank 1
           :section-children (readable-org-fragment-head-content fragment)
           (readable-org-fragment-subheadlines fragment))))

;;;###autoload
(cl-defun readable-save-to-org-file (url file &key allow-overwrite)
  "Save the html content of a web page to a file in Org.

URL is the location of the web page.

FILE is the name of a file to create. It can also be a function
which takes the title of the web page and returns a file name.

If ALLOW-OVERWRITE is non-nil, it will overwrite the latest version
if any."
  (interactive (list (read-string "URL: ")
                     (read-file-name "Save to file: ")))
  (let* ((document (readable-document-for-url url))
         (outfile (cl-etypecase file
                    (string file)
                    (function (funcall file title) )))
         (overwrite (and (file-exists-p outfile)
                         (or allow-overwrite
                             (yes-or-no-p (format "File %s already exists. Overwrite?"
                                                  (abbreviate-file-name outfile)))
                             (user-error "Aborted"))))
         (dom (readable-document-dom document)))
    (with-current-buffer (create-file-buffer outfile)
      (org-ml-insert (point-min)
                     (org-ml-build-headline!
                      :level 1
                      :title-text (oref document title)
                      :section-children
                      (cons (org-ml-build-property-drawer
                             (org-ml-build-node-property "READABLE_ORIGIN_URL" url))
                            (-non-nil
                             (list
                              (when-let (excerpt (oref document excerpt))
                                (org-ml-build-special-block
                                 "excerpt"
                                 (org-ml-build-paragraph! excerpt))))))
                      (readable-org--build-headline dom
                        :title "Fulltext"
                        :tags '("fulltext")
                        :level 1)))
      (setq buffer-file-name outfile)
      (when overwrite
        (delete-file outfile))
      (org-mode)
      (goto-char (point-min))
      (org-show-entry)
      (save-buffer)
      (if (called-interactively-p 'any)
          (pop-to-buffer-same-window (current-buffer))
        (current-buffer)))))

(provide 'readable-org)
;;; readable-org.el ends here
