;;; readable-transient.el --- Transient for readable -*- lexical-binding: t -*-

(require 'readable-document)
(require 'transient)
(require 'url-util)

;;;; URLs (readable--url-variable)

(defclass readable--url-variable (transient-variable)
  ((variable :initarg :variable)))

(cl-defmethod transient-init-value ((obj readable--url-variable))
  (oset obj value (eval (oref obj variable))))

(cl-defmethod transient-infix-set ((obj readable--url-variable) value)
  (oset obj value value)
  (set (oref obj variable) value))

(cl-defmethod transient-format-value ((obj readable--url-variable))
  "")

;;;;; Selecting a URL from the history

(defclass readable--url-history-variable (readable--url-variable)
  ())

(cl-defmethod transient-infix-read ((obj readable--url-history-variable))
  (completing-read "Url from history: "
                   (--map (oref it url) readable-document-tracker)
                   nil nil (oref obj value)))

(transient-define-infix readable-transient-url-history ()
  :class 'readable--url-history-variable
  :variable 'readable-current-url
  :description "Set URL from history")

;;;;; Completing a URL in context

(defclass readable--url-picker-variable (readable--url-variable)
  ((pick :initarg :pick)))

(cl-defmethod transient-infix-read ((obj readable--url-picker-variable))
  (let ((symbol (oref obj pick)))
    (or (funcall symbol)
        (progn
          (message "Returned nil from %s" symbol)
          (read-string "Url: ")))))

(transient-define-infix readable-transient-last-added-url ()
  :class 'readable--url-picker-variable
  :variable 'readable-current-url
  :pick #'readable--last-url
  :description "Last added URL")

(defun readable--last-url ()
  (when-let (doc (car readable-document-tracker))
    (oref doc url)))

(transient-define-infix readable-transient-url-at-point ()
  :class 'readable--url-picker-variable
  :variable 'readable-current-url
  :pick #'readable--url-at-point
  :description "URL at point")

(defun readable--url-at-point ()
  (when-let (url (or (when (derived-mode-p 'org-mode)
                       (-some-> (get-text-property (point) 'htmlize-link)
                         (plist-get :uri)))
                     (thing-at-point 'url)
                     (url-get-url-at-point)))
    (when (string-match-p (rx bos "http" (?  "s") ":") url)
      url)))

(transient-define-infix readable-transient-default-url ()
  :class 'readable--url-picker-variable
  :variable 'readable-current-url
  :pick (lambda () readable-default-url)
  :description "Initial URL")

;;;; Transformation or selecting part of a document

(transient-define-infix readable-peek:document
  
  )


(provide 'readable-transient)
;;; readable-transient.el ends here
