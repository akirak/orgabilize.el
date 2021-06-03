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

(describe "readable-src-lang-from-classes"
  (it "detect a language"
    (expect (readable-src-lang-from-classes "emacs-lisp")
            :to-equal "emacs-lisp"))
  (it "separates the class attribute by white spaces"
    (expect (readable-src-lang-from-classes "heaven-and-hell | emacs-lisp")
            :to-equal "emacs-lisp")
    (expect (readable-src-lang-from-classes "heaven-and-hell\n|\nemacs-lisp\tc")
            :to-equal "emacs-lisp"))
  (it "accepts multiple arguments"
    (expect (readable-src-lang-from-classes
             "heaven-and-hell"
             "|" "just-too-long-to-be-a-mode emacs-lisp another-lengthy-name")
            :to-equal "emacs-lisp")
    (expect (readable-src-lang-from-classes
             ""
             "emacs-lisp")
            :to-equal "emacs-lisp")
    (expect (readable-src-lang-from-classes
             nil
             ""
             "emacs-lisp")
            :to-equal "emacs-lisp"))
  (it "detects a language class that starts with language- prefix"
    (expect (readable-src-lang-from-classes
             "language-emacs-lisp")
            :to-equal "emacs-lisp")
    (expect (readable-src-lang-from-classes
             "heaven-and-hell"
             "md:x-5 | language-c")
            :to-equal "c")))

(describe "readable--file-escape-url-1"
  (it "converts a URL to a path-safe string"
    (expect (readable--file-escape-url-1 "https://github.com/akirak/elinter/blob/v4/README.org")
            :to-be-truthy)
    (expect (readable--file-escape-url-1 "https://www.reddit.com/r/doctorwho/comments/nnm5cq/1_thing_ive_never_understood_about_no_timelords/")
            :to-be-truthy))
  (it "should return the same filename regardless of a trailing slash"
    (expect (readable--file-escape-url-1 "https://github.com")
            :to-equal
            (readable--file-escape-url-1 "https://github.com/"))
    (expect (readable--file-escape-url-1 "https://github.com/akirak")
            :to-equal
            (readable--file-escape-url-1 "https://github.com/akirak/"))))

(describe "readable-url-regexp-for-escaping"
  (it "matches URLs"
    (let ((s "https://security.googleblog.com/2021/05/integrating-rust-into-android-open.html"))
      (expect (and (string-match readable-url-regexp-for-escaping s)
                   (match-string 1 s))
              :to-equal "security.googleblog.com")
      (expect (and (string-match readable-url-regexp-for-escaping s)
                   (match-string 2 s))
              :to-equal "/2021/05/integrating-rust-into-android-open.html")
      (expect (and (string-match readable-url-regexp-for-escaping s)
                   (match-string 3 s))
              :to-be nil))))

(describe "readable--log-url"
  (xit "logs when a new resource is retrieved")
  (xit "logs when the cache hits")
  (xit "logs when a request fails"))

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
