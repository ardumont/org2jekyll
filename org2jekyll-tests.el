(require 'ert)
(require 'el-mock)

(require 'publish)

(ert-deftest test-org2jekyll/get-option-from-file! ()
  (let ((temp-filename "/tmp/test-publish-article-p"))
    (with-temp-file temp-filename  (insert "#+BLOG: tony's blog\n#+DATE: some-date"))
    (should (equal "tony's blog" (org2jekyll/get-option-from-file! temp-filename "BLOG")))
    (should (equal "some-date" (org2jekyll/get-option-from-file! temp-filename "DATE")))
    (should-not (org2jekyll/get-option-from-file! temp-filename "some-other-non-existing-option"))))

(ert-deftest test-org2jekyll/get-option-from-file! ()
  (let ((temp-filename "/tmp/test-publish-article-p"))
    (with-temp-file temp-filename  (insert "#+BLOG: tony's blog\n#+DATE: some-date"))
    (should (equal '(("blog" . "tony's blog")
                     ("date" . "some-date"))
                   (org2jekyll/get-options-from-file! temp-filename '("blog" "date"))))
    (should (equal '(("date" . "some-date"))
                   (org2jekyll/get-options-from-file! temp-filename '("date"))))
    (should (equal '(("unknown"))
                   (org2jekyll/get-options-from-file! temp-filename '("unknown"))))
    (should-not (org2jekyll/get-options-from-file! temp-filename '()))))

(ert-deftest test-org2jekyll/get-option-at-point! ()
  (should (equal "hello"
                 (with-temp-buffer
                   (org-mode)
                   (insert "#+HEADING: hello
#+DATE: some-date")
                   (goto-char (point-min))
                   (org2jekyll/get-option-at-point! "HEADING"))))
  (should (equal "some-date"
                 (with-temp-buffer
                   (org-mode)
                   (insert "#+HEADING: hello
#+DATE: some-date")
                   (goto-char (point-min))
                   (org2jekyll/get-option-at-point! "DATE"))))
  (should-not (with-temp-buffer
                (org-mode)
                (insert "#+HEADING: hello
#+DATE: some-date")
                (goto-char (point-min))
                (org2jekyll/get-option-at-point! "UNKNOWN"))))

(ert-deftest test-org2jekyll/article-p! ()
  (should (let ((temp-filename "/tmp/test-publish-article-p"))
            (with-temp-file temp-filename  (insert "#+BLOG: tony's blog\n#+DATE: some-date"))
            (org2jekyll/article-p! temp-filename)))
  (should-not (let ((temp-filename "/tmp/test-publish-article-p"))
                (with-temp-file temp-filename  (insert "#+NOT-AN-ARTICLE: tony's blog\n#+DATE: some-date"))
                (org2jekyll/article-p! temp-filename))))

(ert-deftest test-org2jekyll/--categories-csv-to-yaml ()
  (should (equal "\n- jabber\n- emacs\n- gtalk\n- tools\n- authentication"  (org2jekyll/--categories-csv-to-yaml "jabber, emacs, gtalk, tools, authentication")))
  (should (equal "\n- jabber\n- emacs\n- gtalk\n- tools\n- authentication"  (org2jekyll/--categories-csv-to-yaml "jabber,emacs,gtalk,tools,authentication"))))

(ert-deftest test-org2jkyll/--to-yaml-header ()
  (should (string= "#+BEGIN_HTML
---
layout: post
title: gtalk in emacs using jabber mode
date: 2013-01-13
author: Antoine R. Dumont
categories:
- jabber
- emacs
- tools
- gtalk
excerpt: Installing jabber and using it from emacs + authentication tips and tricks
---
#+END_HTML
"
                   (org2jekyll/--to-yaml-header '(("layout" . "post")
                                                  ("title" . "gtalk in emacs using jabber mode")
                                                  ("date" . "2013-01-13")
                                                  ("author" . "Antoine R. Dumont")
                                                  ("categories" . "\n- jabber\n- emacs\n- tools\n- gtalk")
                                                  ("description" . "Installing jabber and using it from emacs + authentication tips and tricks"))))))

(ert-deftest test-org2jekyll/--org-to-yaml-metadata ()
  (should (equal '(("layout" . "post")
                   ("title" . "gtalk in emacs using jabber mode")
                   ("date" . "2013-01-13")
                   ("author" . "Antoine R. Dumont")
                   ("categories" . "
- jabber
- emacs
- tools
- gtalk")
                   ("excerpt" . "Installing jabber and using it from emacs + authentication tips and tricks"))
                 (org2jekyll/--org-to-yaml-metadata '(("layout" . "post")
                                                      ("title" . "gtalk in emacs using jabber mode")
                                                      ("date" . "2013-01-13")
                                                      ("author" . "Antoine R. Dumont")
                                                      ("categories" . "\n- jabber\n- emacs\n- tools\n- gtalk")
                                                      ("description" . "Installing jabber and using it from emacs + authentication tips and tricks"))))))

(ert-deftest test-org2jekyll/--convert-timestamp-to-yyyy-dd-mm ()
  (should (equal "2013-04-29" (org2jekyll/--convert-timestamp-to-yyyy-dd-mm "2013-04-29 lun. 00:46"))))
