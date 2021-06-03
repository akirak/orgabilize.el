;;; readable-transient.el --- Transient for readable -*- lexical-binding: t -*-

(require 'readable-document)
(require 'transient)
(require 'url-util)
(require 'goto-addr)

(defsubst readable--http-url-p (url)
  "Return non-nil if URL is an http or https url."
  (string-match-p (rx bos "http" (?  "s") ":") url))

;;;; Documents

(defvar readable-current-document nil)

(defvar goto-address-url-regexp nil)

(defclass readable:document (transient-variable)
  ((selector :initarg :selector :initform nil)
   (init :initarg :init :initform nil)))

(cl-defmethod transient-init-value ((obj readable:document))
  (when-let* ((init (oref obj init))
              (value (funcall init)))
    (oset obj value value)))

(cl-defmethod transient-infix-read ((obj readable:document))
  (or (oref obj value)
      (when-let (selector (oref obj selector))
        (or (funcall selector)
            (progn
              (message "Returned nil from %s" selector)
              nil)))))

(cl-defmethod transient-infix-set ((obj readable:document) value)
  (when value
    (setq readable-current-document value)))

(cl-defmethod transient-format-value ((obj readable:document))
  (if-let (value (oref obj value))
      (format "(%s)" (oref value url))
    ""))

;;;;; Selecting a document interactively

(transient-define-infix readable:document-history ()
  :description "Set document from history"
  :class 'readable:document
  :selector (lambda ()
              (let ((url (completing-read
                          "Url from history: "
                          (--map (oref it url) readable-document-tracker)
                          nil nil nil nil
                          (when readable-current-document
                            (oref readable-current-document url)))))
                (ignore-errors
                  (readable-document-for-url url)))))

(transient-define-infix readable:url-from-kill-ring ()
  :description "Set URL from kill ring"
  :class 'readable:document
  :selector (lambda ()
              (let ((url (completing-read "Url: "
                                          (readable--urls-from-selection))))
                (ignore-errors
                  (readable-document-for-url url)))))

(defun readable--urls-from-selection ()
  "Return a list of URLs contained in the selection."
  (--> (funcall interprogram-paste-function)
    (if (listp it) it (list it))
    (append it kill-ring)
    (-map #'clipurl--urls-in-string it)
    (-flatten-n 1 it)))

(defun readable--urls-in-string (string)
  "Extract URLs from STRING"
  (->> string
    (s-match-strings-all goto-address-url-regexp)
    (-flatten-n 1)
    (-filter #'readable--http-url-p url)))

;;;;; Completing a URL in context

(transient-define-infix readable:last-added-document ()
  :description "Last added document"
  :class 'readable:document
  :init #'readable--last-added-document)

(defun readable--last-added-document ()
  (car readable-document-tracker))

(transient-define-infix readable:document-at-point ()
  :description "URL at point"
  :class 'readable:document
  :init #'readable--document-at-point)

(defun readable--document-at-point ()
  (when-let (url (readable--url-at-point))
    (readable-document-for-url url)))

(defun readable--url-at-point ()
  (let ((url (or (when (derived-mode-p 'org-mode)
                   (-some-> (get-text-property (point) 'htmlize-link)
                     (plist-get :uri)))
                 (thing-at-point 'url)
                 (url-get-url-at-point))))
    (when (and url (readable--http-url-p url))
      url)))

(provide 'readable-transient)
;;; readable-transient.el ends here
