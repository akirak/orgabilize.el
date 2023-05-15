;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'orgabilize)

(describe "Utilities"
  (describe "orgabilize--string-take"
    (it "Takes the first N characters of the string"
      (expect (orgabilize--string-take 5 "Hello")
              :to-equal "Hello")
      (expect (orgabilize--string-take 5 "Hi")
              :to-equal "Hi")
      (expect (orgabilize--string-take 5 "Good bye")
              :to-equal "Good "))))

(describe "Fragments"
  (describe "orgabilize-document-fragment-title"
    (it "returns the heading of a fragment with the ID"
      (expect (orgabilize-document-fragment-title
               "https://cabal.readthedocs.io/en/3.4/cabal-project.html"
               "specifying-the-local-packages")
              :to-equal "8.1. Specifying the local packages")
      (expect (orgabilize-document-fragment-title
               "https://en.wikipedia.org/wiki/Logic"
               "Systems_of_logic")
              :to-equal "Systems of logic[edit]")
      (expect (orgabilize-document-fragment-title
               "https://v2.ocaml.org/manual/expr.html"
               "ss:expr-operators")
              :to-equal "7.5 Operators")
      (expect (orgabilize-document-fragment-title
               "https://hexdocs.pm/phoenix/channels.html"
               "client-libraries")
              :to-equal "client-libraries Client Libraries")
      (expect (orgabilize-document-fragment-title
               "https://github.com/elixir-desktop/desktop/blob/main/guides/getting_started.md"
               "gnulinux")
              :to-be nil))))

(provide 'orgabilize-test)
