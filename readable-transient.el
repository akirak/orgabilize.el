;;; readable-transient.el --- Transient for readable -*- lexical-binding: t -*-

(require 'readable-document)
(require 'transient)
(require 'url-util)

;;;; Documents

(defvar readable-current-document nil)

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

;;;;; Selecting a document from the history

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
    (when (and url (string-match-p (rx bos "http" (?  "s") ":") url))
      url)))

;;;; Transformation or selecting part of a document

(defvar readable-current-transformation nil)

(provide 'readable-transient)
;;; readable-transient.el ends here
