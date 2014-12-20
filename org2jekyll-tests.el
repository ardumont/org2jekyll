(require 'ert)
(require 'el-mock)

(require 'cl)
(require 'org2jekyll)

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

(ert-deftest test-org2jekyll/--to-yaml-header ()
  (should (string= "#+BEGIN_HTML
---
layout: post
title: gtalk in emacs using jabber mode
date: 2013-01-13
author: Antoine R. Dumont
categories: \n- jabber
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

(ert-deftest test-org2jekyll/--compute-ready-jekyll-file-name ()
  (should (equal "/home/tony/org/2012-10-10-scratch.org"
                 (org2jekyll/--compute-ready-jekyll-file-name "2012-10-10" "/home/tony/org/scratch.org"))))
(require 'el-mock)
(ert-deftest test-org2jekyll/--copy-org-file-to-jekyll-org-file ()
  (should (equal "#+BEGIN_HTML
---
layout: post
title: some fake title
date: 2012-10-10
categories: \n- some-fake-category1\n- some-fake-category2
author: some-fake-author
excerpt: some-fake-description with spaces and all
---
#+END_HTML
#+fake-meta: some fake meta
* some content"
                 (let ((fake-date            "2012-10-10")
                       (fake-org-file        "/tmp/scratch.org")
                       (fake-org-jekyll-file "/tmp/fake-org-jekyll.org"))
                   ;; @before
                   (when (file-exists-p fake-org-file) (delete-file fake-org-file))
                   (when (file-exists-p fake-org-jekyll-file) (delete-file fake-org-jekyll-file))
                   ;; @Test
                   (mocklet (((org2jekyll/--compute-ready-jekyll-file-name fake-date fake-org-file) => fake-org-jekyll-file))
                     ;; create fake org file with some default content
                     (with-temp-file fake-org-file
                       (insert "#+fake-meta: some fake meta\n* some content"))
                     ;; create the fake jekyll file with jekyll metadata
                     (--> fake-org-file
                       (org2jekyll/--copy-org-file-to-jekyll-org-file fake-date it `(("layout"      . "post")
                                                                                     ("title"       . "some fake title")
                                                                                     ("date"        . ,fake-date)
                                                                                     ("categories"  . "\n- some-fake-category1\n- some-fake-category2")
                                                                                     ("author"      . "some-fake-author")
                                                                                     ("description" . "some-fake-description with spaces and all")))
                       (insert-file-contents it)
                       (with-temp-buffer it (buffer-string))))))))

(ert-deftest test-org2jekyll/--convert-timestamp-to-yyyy-dd-mm ()
  (should (equal "2013-04-29" (org2jekyll/--convert-timestamp-to-yyyy-dd-mm "2013-04-29 lun. 00:46"))))
