;;; readable-transient.el --- Transient for readable -*- lexical-binding: t -*-

(require 'readable-document)
(require 'transient)
(require 'url-util)

;;;; URLs (readable--url-variable)

(defvar readable-current-url nil)

(defclass readable--url-variable (transient-variable)
  ((selector :initarg :selector :initform nil)
   (init :initarg :init :initform nil)))

(cl-defmethod transient-init-value ((obj readable--url-variable))
  (when-let* ((init (oref obj init))
              (value (funcall init)))
    (oset obj value value)))

(cl-defmethod transient-infix-read ((obj readable--url-variable))
  (or (oref obj value)
      (when-let (selector (oref obj selector))
        (or (funcall selector)
            (progn
              (message "Returned nil from %s" selector)
              nil)))))

(cl-defmethod transient-infix-set ((obj readable--url-variable) value)
  (when value
    (setq readable-current-url value)))

(cl-defmethod transient-format-value ((obj readable--url-variable))
  (if-let (value (oref obj value))
      (format "(%s)" value)
    ""))

;;;;; Selecting a URL from the history

(transient-define-infix readable:url-history ()
  :description "Set URL from history"
  :class 'readable--url-variable
  :selector (lambda ()
              (completing-read "Url from history: "
                               (--map (oref it url) readable-document-tracker)
                               nil nil nil nil readable-current-url)))

;;;;; Completing a URL in context

(transient-define-infix readable:last-added-url ()
  :description "Last added URL"
  :class 'readable--url-variable
  :init #'readable--last-url)

(defun readable--last-url ()
  (when-let (doc (car readable-document-tracker))
    (oref doc url)))

(transient-define-infix readable:url-at-point ()
  :description "URL at point"
  :class 'readable--url-variable
  :init #'readable--url-at-point)

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
