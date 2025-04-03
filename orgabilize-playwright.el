;;; orgabilize-playwright.el --- Playwright backend for scraping web pages -*- lexical-binding: t -*-

;; Copyright (C) 2025 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (mcp "0.1"))
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

;; This library provides a backend for scraping web pages. For configuration,
;; see `orgabilize-use-playwright'.

;;; Code:

(require 'mcp)

(defgroup orgabilize-playwright nil
  "Playwright backend for orgabilize."
  :group 'orgabilize)

(defcustom orgabilize-playwright-mcp-name "playwright"
  "Name of the MCP server for Playwright.

This should be the name of an entry in `mcp-hub-servers'. The server
should run <https://github.com/microsoft/playwright-mcp>."
  :group 'orgabilize-playwright
  :type 'string)

(defun orgabilize-playwright--get-connection ()
  "Return an active connection of the MCP server."
  (and orgabilize-playwright-mcp-name
       (gethash orgabilize-playwright-mcp-name mcp-server-connections)))

(defun orgabilize-playwright-get-title (url)
  (if-let* ((connection (orgabilize-playwright--get-connection)))
      (with-temp-buffer
        (insert (orgabilize-playwright--mcp-snapshot connection url))
        (goto-char (point-min))
        (orgabilize-playwright--parse-title url))
    (message "Playwright MCP is not configured or disconnected.\
See `orgabilize-playwright-mcp-name'")
    nil))

(defun orgabilize-playwright--mcp-snapshot (connection url)
  (mcp-call-tool connection "browser_navigate" (list :url url))
  (thread-first
    (unwind-protect
        (mcp-call-tool connection "browser_snapshot" nil)
      (mcp-call-tool connection "browser_close" nil))
    (plist-get :content)
    (elt 0)
    (plist-get :text)))

(defun orgabilize-playwright--parse-title (url)
  (let ((case-fold-search t))
    (re-search-forward (rx "page title:" (* blank)))
    (buffer-substring-no-properties (point) (line-end-position))))

(provide 'orgabilize-playwright)
;;; orgabilize-playwright.el ends here
