;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'readable)

(describe "Utilities"
  (describe "readable--string-take"
    (it "Takes the first N characters of the string"
      (expect (readable--string-take 5 "Hello")
              :to-equal "Hello")
      (expect (readable--string-take 5 "Hi")
              :to-equal "Hi")
      (expect (readable--string-take 5 "Good bye")
              :to-equal "Good ")))

  (describe "readable--origin-buffer"
    (let ((readable-cache-directory (make-temp-file "readable" 'dir)))
      (unwind-protect
          (let ((buf1 (readable--origin-buffer "https://web.dev")))
            (progn
              (it "Returns a read-only buffer of the content"
                (expect (bufferp buf1) :to-be-truthy)
                (expect (buffer-local-value 'buffer-read-only buf1) :to-be-truthy)
                (expect (with-current-buffer buf1
                          (buffer-substring (point-min) 20))
                        :to-match
                        (rx bol "<!DOCTYPE html>"))))
            (it "Returns the same buffer if called repeatedly"
              (expect (readable--origin-buffer "https://web.dev")
                      :to-equal buf1)))
        (delete-directory readable-cache-directory 'recursive)))))

(defun readable-test-read-html (file url)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (readable--html-content url)))

(describe "HTML DOM analysis"
  (describe "readable-html-from-xml"
    (it "analyses dom"
      (expect (readable-html-from-xml
               (readable-test-read-html
                "data/readable/mdn-1.html"
                "https://developer.mozilla.org/en-US/docs/Web/API/CSS_Object_Model/Managing_screen_orientation"))
              :to-be-truthy))))

(provide 'readable-test)
