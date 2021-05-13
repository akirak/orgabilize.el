;;; readable-helpers.el --- Miscellenaous helpers -*- lexical-binding: t -*-

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

;; This library provides miscellaneous utilities for readable.el.

;; For now, it contains a helper for source blocks,
;; `readable-src-lang-from-classes'.

;;; Code:

(defcustom readable-src-lang-class-regexp
  (rx bos "language-" (group (+ anything)) eos)
  "Regexp for detecting a language name from an HTML class.

The first group should match the name of a language."
  :group 'readable
  :type 'string)

(defun readable--normalize-html-classes (class-strings)
  "Return a list of html class names from CLASS-STRINGS."
  (->> class-strings
    (-non-nil)
    (--map (split-string it))
    (-flatten-n 1)
    ;; Exclude items that doesn't contain an alphabet, e.g. "|" for separator
    (--filter (string-match-p "[[:alpha:]]" it))))

(defun readable--conventional-src-classes (class-names)
  "Return a list of names that are likely to mention a source language.

This takes CLASS-NAMES which is a list of classes and possibly
returns some source language names. Such a class name typically
starts with a prefix \"language-\", and the prefix will be
removed in that case.

For customization, see `readable-src-lang-class-regexp'."
  (->> class-names
    (--map (save-match-data
             (when (string-match readable-src-lang-class-regexp it)
               (match-string 1 it))))
    (-non-nil)))

(defun readable--src-language-p (string)
  "Return non-nil if STRING denotes a source language."
  (when-let (mode (intern-soft (concat string "-mode")))
    (commandp mode)))

(defun readable-src-lang-from-classes (classes &rest more-classes)
  "Find a source language from class attributes in html.

CLASSES is a space-separated list of class names in an html
element. Alternatively, you can specify MORE-CLASSES from
multiple elements, for checking both a pre element and a code
element inside it."
  (let* ((nclasses (readable--normalize-html-classes (cons classes more-classes)))
         (classes1 (readable--conventional-src-classes nclasses)))
    (->> (append classes1 nclasses)
      (-uniq)
      (-find #'readable--src-language-p))))

(provide 'readable-helpers)
;;; readable-helpers.el ends here
