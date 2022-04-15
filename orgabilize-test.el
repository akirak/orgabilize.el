;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'readable)

(describe "Utilities"
  (describe "orgabilize--string-take"
    (it "Takes the first N characters of the string"
      (expect (orgabilize--string-take 5 "Hello")
              :to-equal "Hello")
      (expect (orgabilize--string-take 5 "Hi")
              :to-equal "Hi")
      (expect (orgabilize--string-take 5 "Good bye")
              :to-equal "Good ")))

  (describe "orgabilize--origin-buffer"
    (let ((orgabilize-cache-directory (make-temp-file "readable" 'dir)))
      (unwind-protect
          (let ((buf1 (orgabilize--origin-buffer "https://web.dev")))
            (progn
              (it "Returns a read-only buffer of the content"
                (expect (bufferp buf1) :to-be-truthy)
                (expect (buffer-local-value 'buffer-read-only buf1) :to-be-truthy)
                (expect (with-current-buffer buf1
                          (buffer-substring (point-min) 20))
                        :to-match
                        (rx bol "<!DOCTYPE html>"))))
            (it "Returns the same buffer if called repeatedly"
              (expect (orgabilize--origin-buffer "https://web.dev")
                      :to-equal buf1)))
        (delete-directory orgabilize-cache-directory 'recursive)))))

(defun orgabilize-test-read-html (file url)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (orgabilize--html-content url)))

(describe "HTML DOM analysis"
  (describe "orgabilize-html-from-xml"
    (it "analyses dom"
      (expect (orgabilize-html-from-xml
               (orgabilize-test-read-html
                "data/readable/mdn-1.html"
                "https://developer.mozilla.org/en-US/docs/Web/API/CSS_Object_Model/Managing_screen_orientation"))
              :to-be-truthy))))

(provide 'orgabilize-test)
