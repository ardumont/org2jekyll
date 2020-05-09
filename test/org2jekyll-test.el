;;; org2jekyll-test.el --- Test org2jekyll
;;; Commentary:

(require 'ert)
(require 'el-mock)

;;; Code:
(ert-deftest test-org2jekyll-get-options-from-buffer ()
  (let* ((blog-key "#+BLOG:")
         (blog-val "tony's blog")
         (date-key "#+DATE:")
         (date-val "some-date")
         (options-plist (with-temp-buffer (insert (concat blog-key " " blog-val "\n"
                                                          date-key " " date-val "\n"
                                                          "Beef fungus articles"))
                                          (org2jekyll-get-options-from-buffer))))
    (should (string= blog-val (plist-get options-plist :blog)))
    (should (string= date-val (plist-get options-plist :date)))))

(ert-deftest test-org2jekyll-get-options-from-file ()
  (let* ((temp-file "/tmp/test-get-options-from-file")
         (blog-key "#+BLOG:")
         (blog-val "tony's blog")
         (date-key "#+DATE:")
         (date-val "some-date")
         (_ (with-temp-file temp-file (insert (concat blog-key " " blog-val "\n"
                                                      date-key " " date-val "\n"
                                                      "Beef fungus articles"))))
         (options-plist (org2jekyll-get-options-from-file temp-file))
         (_ (delete-file temp-file)))
    (should (string= blog-val (plist-get options-plist :blog)))
    (should (string= date-val (plist-get options-plist :date)))))

(ert-deftest test-org2jekyll-article-p ()
  (let* ((temp-file "/tmp/test-org2jekyll-get-article-p")
         (layout-key "#+LAYOUT:") (layout-val "post")
         (date-key "#+DATE:") (date-val "some-date")
         (not-article-key "#+NOT-AN-ARTICLE:") (not-article-val "tony's blog")
         (_ (with-temp-file temp-file (insert (concat layout-key " " layout-val "\n"
                                                      date-key " " date-val "\n"))))
         (article (org2jekyll-article-p temp-file))
         (_ (delete-file temp-file))
         (_ (with-temp-file temp-file (insert (concat not-article-key " " not-article-val "\n"))))
         (not-article (org2jekyll-article-p temp-file))
         (_ (delete-file temp-file)))
    (should article)
    (should-not not-article)))



(ert-deftest test-org2jekyll-make-slug ()
  (should (string= "this-is-a-test"
                   (org2jekyll--make-slug "this-is-a-test")))
  (should (string= "forbidden-symbol"
                   (org2jekyll--make-slug "forb~idd^en-symbol![](){}$#")))
  (should (string= "你好-test"
                   (org2jekyll--make-slug "你好-test")))
  (should (string= "你好-большие-test"
                   (org2jekyll--make-slug "你好-большие-test"))))

(ert-deftest test-org2jekyll--draft-filename ()
  (let ((org2jekyll-jekyll-post-ext ".ext"))
    (should (string= "/some/path/你好-test.ext"
                     (org2jekyll--draft-filename "/some/path/" "你好-test")))
    (should (string= "/some/path/你好-большие-test.ext"
                     (org2jekyll--draft-filename "/some/path/" "你好-Большие-test")))
    (should (string= "/some/path/forbidden-symbol.ext"
                     (org2jekyll--draft-filename "/some/path/" "forbidden-symbol\\![](){}^$#")))))

(ert-deftest test-org2jekyll--csv-to-yaml ()
  (should (string= "\n- jabber\n- emacs\n- gtalk\n- tools\n- authentication"  (org2jekyll--csv-to-yaml "jabber, emacs, gtalk, tools, authentication")))
  (should (string= "\n- jabber\n- emacs\n- gtalk\n- tools\n- authentication"  (org2jekyll--csv-to-yaml "jabber,emacs,gtalk,tools,authentication"))))

(ert-deftest org2jekyll--old-org-version-p ()
  ;; pre-9.0 Org releases
  (should (mocklet (((boundp *) => t))
            (org2jekyll--old-org-version-p)))
  ;; Org 9.0+ and org 8.3.x git snapshots
  (should-not (mocklet (((boundp *) => nil))
                (org2jekyll--old-org-version-p))))

(ert-deftest test-org2jekyll--to-yaml-header ()
  ;; pre-9.0 Org releases
  (should (string= "#+BEGIN_HTML
---
layout: post
title: gtalk in emacs using jabber mode
date: 2013-01-13
author: Antoine R. Dumont
categories: \n- jabber\n- emacs\n- tools\n- gtalk
tags: \n- tag0\n- tag1\n- tag2
excerpt: Installing jabber and using it from emacs + authentication tips and tricks
comments: true
permalink: /posts/gtalk/
---
#+END_HTML
"
                   (mocklet (((org2jekyll--old-org-version-p) => t))
                     (org2jekyll--to-yaml-header '(("layout" . "post")
                                                   ("title" . "gtalk in emacs using jabber mode")
                                                   ("date" . "2013-01-13")
                                                   ("author" . "Antoine R. Dumont")
                                                   ("categories" . "\n- jabber\n- emacs\n- tools\n- gtalk")
                                                   ("tags"  . "\n- tag0\n- tag1\n- tag2")
                                                   ("description" . "Installing jabber and using it from emacs + authentication tips and tricks")
                                                   ("comments" . "true")
                                                   ("permalink" . "/posts/gtalk/"))))))
  ;; Org 9.0+ and org 8.3.x git snapshots
  (should (string= "#+BEGIN_EXPORT HTML
---
layout: post
title: gtalk in emacs using jabber mode
date: 2013-01-13
author: Alexey Kopytov
categories: \n- jabber\n- emacs\n- tools\n- gtalk
tags: \n- tag0\n- tag1\n- tag2
excerpt: Installing jabber and using it from emacs + authentication tips and tricks
comments: true
permalink: /posts/gtalk/
---
#+END_EXPORT
"
                   (mocklet (((org2jekyll--old-org-version-p) => nil))
                     (org2jekyll--to-yaml-header '(("layout" . "post")
                                                   ("title" . "gtalk in emacs using jabber mode")
                                                   ("date" . "2013-01-13")
                                                   ("author" . "Alexey Kopytov")
                                                   ("categories" . "\n- jabber\n- emacs\n- tools\n- gtalk")
                                                   ("tags"  . "\n- tag0\n- tag1\n- tag2")
                                                   ("description" . "Installing jabber and using it from emacs + authentication tips and tricks")
                                                   ("comments" . "true")
                                                   ("permalink" . "/posts/gtalk/")))))))

(ert-deftest test-org2jekyll--org-to-jekyll-metadata ()
  (should (equal '(("layout" . "post")
                   ("title" . "gtalk in emacs using jabber mode")
                   ("date" . "2013-01-13")
                   ("author" . "Antoine R. Dumont")
                   ("categories" . "
- jabber
- emacs
- tools
- gtalk")
                   ("excerpt" . "Installing jabber and using it from emacs + authentication tips and tricks")

                   ("comments" . "true")
                   ("permalink" . "/posts/gtalk/"))
                 (org2jekyll--org-to-jekyll-metadata '(("layout" . "post")
                                                       ("title" . "gtalk in emacs using jabber mode")
                                                       ("date" . "2013-01-13")
                                                       ("author" . "Antoine R. Dumont")
                                                       ("categories" . "\n- jabber\n- emacs\n- tools\n- gtalk")
                                                       ("description" . "Installing jabber and using it from emacs + authentication tips and tricks")
                                                       ("comments" . "true")
                                                       ("permalink" . "/posts/gtalk/"))))))

(ert-deftest test-org2jekyll--compute-ready-jekyll-file-name ()
  (should (string= "/home/tony/org/2012-10-10-scratch.org"
                   (org2jekyll--compute-ready-jekyll-file-name "2012-10-10" "/home/tony/org/scratch.org")))
  (should (string= "/home/tony/org/2012-10-10-scratch.org"
                   (let* ((fake-drafts-folder "fake-drafts-folder")
                          (org2jekyll-jekyll-drafts-dir fake-drafts-folder))
                     (org2jekyll--compute-ready-jekyll-file-name "2012-10-10" (format "/home/tony/org/%s/scratch.org" fake-drafts-folder))))))

(ert-deftest test-org2jekyll--copy-org-file-to-jekyll-org-file ()
  ;; pre-9.0 Org releases
  (should (string= "#+BEGIN_HTML
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
                     (mocklet (((org2jekyll--old-org-version-p) => t)
                               ((org2jekyll--compute-ready-jekyll-file-name fake-date fake-org-file) => fake-org-jekyll-file))
                       ;; create fake org file with some default content
                       (with-temp-file fake-org-file
                         (insert "#+fake-meta: some fake meta\n* some content"))
                       ;; create the fake jekyll file with jekyll metadata
                       (--> fake-org-file
                            (org2jekyll--copy-org-file-to-jekyll-org-file
                             fake-date it `(("layout"      . "post")
                                            ("title"       . "some fake title")
                                            ("date"        . ,fake-date)
                                            ("categories"  . "\n- some-fake-category1\n- some-fake-category2")
                                            ("author"      . "some-fake-author")
                                            ("description" . "some-fake-description with spaces and all")))
                            (insert-file-contents it)
                            (buffer-string)))))))

(ert-deftest test-org2jekyll--copy-org-file-to-jekyll-org-file-2 ()
  ;; Org 9.0+ and org 8.3.x git snapshots
  (should (string= "#+BEGIN_EXPORT HTML
---
layout: post
title: fake title
date: 2016-02-28
categories: \n- fake-category1\n- fake-category2
author: fake-author
excerpt: fake-description with spaces and all
---
#+END_EXPORT
#+fake-meta: fake meta
* some content"
                   (let ((fake-date            "2016-02-28")
                         (fake-org-file        "/tmp/scratch-9.0.org")
                         (fake-org-jekyll-file "/tmp/fake-org-jekyll-9.0.org"))
                     ;; @before
                     (when (file-exists-p fake-org-file) (delete-file fake-org-file))
                     (when (file-exists-p fake-org-jekyll-file) (delete-file fake-org-jekyll-file))
                     ;; @Test
                     (mocklet (((org2jekyll--old-org-version-p) => nil)
                               ((org2jekyll--compute-ready-jekyll-file-name fake-date fake-org-file) => fake-org-jekyll-file))
                       ;; create fake org file with some default content
                       (with-temp-file fake-org-file
                         (insert "#+fake-meta: fake meta\n* some content"))
                       ;; create the fake jekyll file with jekyll metadata
                       (--> fake-org-file
                            (org2jekyll--copy-org-file-to-jekyll-org-file
                             fake-date it `(("layout"      . "post")
                                            ("title"       . "fake title")
                                            ("date"        . ,fake-date)
                                            ("categories"  . "\n- fake-category1\n- fake-category2")
                                            ("author"      . "fake-author")
                                            ("description" . "fake-description with spaces and all")))
                            (insert-file-contents it)
                            (buffer-string)))))))

(ert-deftest test-org2jekyll--convert-timestamp-to-yyyy-dd-mm ()
  (should (equal "2013-04-29" (org2jekyll--convert-timestamp-to-yyyy-dd-mm "2013-04-29 lun. 00:46"))))

(ert-deftest test-org2jekyll-assoc-default ()
  (should (string= "default-value"  (org2jekyll-assoc-default nil nil "default-value")))
  (should (string= "default-value"  (org2jekyll-assoc-default "key" '(("key". nil))  "default-value")))
  (should (string= "original-value" (org2jekyll-assoc-default "key" '(("key". "original-value")) "default-value")))
  (should (string= "default-value"  (org2jekyll-assoc-default "key" '(("key")) "default-value"))))

(ert-deftest test-org2jekyll-remove-org-only-options ()
  (let* ((test-options '(("startup" . "hidestars")
                         ("options" . "toc:nil")
                         ("author" . "me")
                         ("date" . "2015-12-23 Sat 14:20")
                         ("title" . "some-title")))
         (expected-results '(("author" . "me")
                             ("date" . "2015-12-23 Sat 14:20")
                             ("title" . "some-title")))
         (jekyll-options (org2jekyll-remove-org-only-options test-options)))
    (should (equal expected-results jekyll-options))))

(ert-deftest test-org2jekyll-read-metadata ()
  (let* ((temp-file "/tmp/test-org2jekyll-read-metadata")
         (startup-key "#+STARTUP:") (startup-val "hidestars")
         (options-key "#+OPTIONS:") (options-val "H:2 num:nil tags:nil toc:nil timestamps:t")
         (layout-key "#+LAYOUT:") (layout-val "default")
         (author-key "#+AUTHOR:") (author-val "me")
         (date-key "#+DATE:") (date-val "2015-12-23 Sat 14:20")
         (title-key "#+TITLE:") (title-val "some-title")
         (description-key "#+DESCRIPTION:") (description-val "desc")
         (tags-key "#+TAGS:") (tags-val "tag0, tag1")
         (categories-key "#+CATEGORIES:") (categories-val "cat0, cat1")
         (comments-key "#+COMMENTS:") (comments-val "true")
         (_ (with-temp-file temp-file
              (insert (concat startup-key " " startup-val "\n"
                              options-key " " options-val "\n"
                              layout-key " " layout-val "\n"
                              title-key " " title-val "\n"
                              date-key " " date-val "\n"
                              categories-key " " categories-val "\n"
                              tags-key " " tags-val "\n"
                              author-key " " author-val "\n"
                              description-key " " description-val "\n"
                              comments-key " " comments-val "\n"))))
         (options-alist (org2jekyll-read-metadata temp-file))
         (_ (delete-file temp-file)))
    (should (string= "default" (assoc-default "layout" options-alist)))
    (should (string= "some-title" (assoc-default "title" options-alist)))
    (should (string= "2015-12-23" (assoc-default "date" options-alist)))
    (should (string= "\n- cat0\n- cat1" (assoc-default "categories" options-alist)))
    (should (string= "\n- tag0\n- tag1" (assoc-default "tags" options-alist)))
    (should (string= "me" (assoc-default "author" options-alist)))
    (should (string= "desc" (assoc-default "description" options-alist)))
    (should (string= "true" (assoc-default "comments" options-alist)))
    (should (null (assoc-default "startup" options-alist)))
    (should (null (assoc-default "options" options-alist)))))

(ert-deftest test-org2jekyll-read-metadata-required-headers ()
  (let* ((temp-file "/tmp/test-org2jekyll-read-metadata-required-headers")
         (layout-key "#+LAYOUT:") (layout-val "post")
         (description-key "#+DESCRIPTION:") (description-val "desc")
         (_ (with-temp-file temp-file
              (insert (concat layout-key " " layout-val "\n"
                              description-key " " description-val "\n"))))
         (options-alist (org2jekyll-read-metadata temp-file))
         (_ (delete-file temp-file)))
    (should (string= "This org-mode file is missing required header(s):
- The title is required, please add '#+TITLE' at the top of your org buffer.
- The categories is required, please add '#+CATEGORIES' at the top of your org buffer.
Publication skipped" options-alist))))

(ert-deftest test-org2jekyll-default-headers-template ()
  (should (string= "#+STARTUP: showall
#+STARTUP: hidestars
#+OPTIONS: H:2 num:nil tags:nil toc:nil timestamps:t
#+LAYOUT: some-layout
#+AUTHOR: blog-author
#+DATE: post-date
#+TITLE: post title with spaces
#+DESCRIPTION: post some description
#+TAGS: post-tag0, post-tag1
#+CATEGORIES: post-category, other-category

"
                   (org2jekyll-default-headers-template "some-layout"
                                                        "blog-author"
                                                        "post-date"
                                                        "post title with spaces"
                                                        "post some description"
                                                        "post-tag0, post-tag1"
                                                        "post-category, other-category"))))

(ert-deftest test-org2jekyll--optional-folder ()
  (should (string= "hello/there" (org2jekyll--optional-folder "hello" "there")))
  (should (string= "hello/" (org2jekyll--optional-folder "hello"))))

(ert-deftest test-org2jekyll-input-directory ()
  (let ((org2jekyll-source-directory "source-directory"))
    (should (string= "source-directory/there" (org2jekyll-input-directory "there")))
    (should (string= "source-directory/" (org2jekyll-input-directory)))))

(ert-deftest test-org2jekyll-output-directory ()
  (let ((org2jekyll-jekyll-directory "out-directory"))
    (should (string= "out-directory/there" (org2jekyll-output-directory "there")))
    (should (string= "out-directory/" (org2jekyll-output-directory)))))

(ert-deftest test-org2jekyll-read-metadata-and-execute ()
  (should (string= "Post 'some-org-file' published!"
                 (mocklet (((org2jekyll-article-p "org-file") => t)
                           ((file-name-nondirectory "org-file") => "some-org-file")
                           ((org2jekyll-read-metadata "org-file") => '(("layout" . "post"))))
                   (org2jekyll-read-metadata-and-execute (lambda (org-metadata org-file) 2) "org-file"))))
  (should (string= "'some-org-file' is not an article, publication skipped!"
                 (mocklet (((org2jekyll-article-p "org-file") => nil)
                           ((file-name-nondirectory "org-file") => "some-org-file"))
                   (org2jekyll-read-metadata-and-execute (lambda (org-metadata org-file) 2) "org-file"))))
  (should (string= "org2jekyll - some message"
                 (mocklet (((org2jekyll-article-p "org-file") => t)
                           ((org2jekyll-read-metadata "org-file") => "some message"))
                   (org2jekyll-read-metadata-and-execute (lambda (org-metadata org-file) 2) "org-file")))))

(ert-deftest test-org2jekyll--publish-post-org-file-with-metadata ()
  (should (eq :file-deleted
              (let ((org-publish-project-alist '((:post :project))))
                (with-mock
                  (mock (org2jekyll--copy-org-file-to-jekyll-org-file :date :org-file '(("layout" . :post)
                                                                                        ("date" . :date))) => :jekyll-file)
                  (mock (org-publish-file :jekyll-file '(:post :project)) => :published-file)
                  (mock (delete-file :jekyll-file) => :file-deleted)
                  (org2jekyll--publish-post-org-file-with-metadata '(("layout" . :post)
                                                                     ("date" . :date)) :org-file))))))

(ert-deftest test-org2jekyll-post-p ()
  (should (org2jekyll-post-p "post"))
  (should-not (org2jekyll-post-p "default")))

(ert-deftest test-org2jekyll-page-p ()
  (should (org2jekyll-page-p "default"))
  (should-not (org2jekyll-page-p "post")))

(ert-deftest test-org2jekyll-check-metadata ()
  (let* ((temp-file "/tmp/test-org2jekyll-check-metadata")
         (startup-key "#+STARTUP:") (startup-val "hidestars")
         (options-key "#+OPTIONS:") (options-val "H:2 num:nil tags:nil toc:nil timestamps:t")
         (layout-key "#+LAYOUT:") (layout-val "some-layout")
         (author-key "#+AUTHOR:") (author-val "blog-author")
         (date-key "#+DATE:") (date-val "post-date")
         (title-key "#+TITLE:") (title-val "post title with spaces")
         (description-key "#+DESCRIPTION:") (description-val "post some description")
         (tags-key "#+TAGS:") (tags-val "post-tag0, post-tag1")
         (categories-key "#+CATEGORIES:") (categories-val "post-category, other-category")
         (_ (with-temp-file temp-file
              (insert (concat startup-key " " startup-val "\n"
                              options-key " " options-val "\n"
                              layout-key " " layout-val "\n"
                              author-key " " author-val "\n"
                              date-key " " date-val "\n"
                              title-key " " title-val "\n"
                              description-key " " description-val "\n"
                              tags-key " " tags-val "\n"
                              categories-key " " categories-val "\n"))))
         (options-plist (org2jekyll-get-options-from-file temp-file))
         (_ (delete-file temp-file))
         (metadata-errors (org2jekyll-check-metadata options-plist)))
    (should (null metadata-errors)))
  (should (string= "- The title is required, please add '#+TITLE' at the top of your org buffer.
- The categories is required, please add '#+CATEGORIES' at the top of your org buffer.
- The description is required, please add '#+DESCRIPTION' at the top of your org buffer.
- The layout is required, please add '#+LAYOUT' at the top of your org buffer."
                 (org2jekyll-check-metadata nil)))
  (should (string= "- The categories is required, please add '#+CATEGORIES' at the top of your org buffer.
- The description is required, please add '#+DESCRIPTION' at the top of your org buffer."
                 (org2jekyll-check-metadata '(:title "some-title"
                                                     :layout "some-layout"))))
  (should-not (org2jekyll-check-metadata '(:title "some-title"
                                                  :layout "some-layout"
                                                  :author "some-author"
                                                  :categories "some-categories"
                                                  :description "some-description"))))

(ert-deftest test-org2jekyll--yaml-escape ()
  (should (string= "this is a title"
                   (org2jekyll--yaml-escape "this is a title")))
  (should (string= "\"title:\""
                   (org2jekyll--yaml-escape "title:")))
  (should (string= "\"\\\"title:\\\"\""
                   (org2jekyll--yaml-escape "\"title:\""))))

(ert-deftest test-org2jekyll--list-dir ()
  (should (equal :found
                 (with-mock (mock (find-file "found-file") => :found)
                            (org2jekyll--list-dir "found-file"))))
  (should-not (with-mock (mock (find-file "unknown") => nil)
                         (org2jekyll--list-dir "unknown"))))

(ert-deftest test-org2jekyll-list-posts ()
  (should (equal :found
                 (let ((org2jekyll-jekyll-directory "path-to-jekyll-root")
                       (org2jekyll-jekyll-posts-dir "some-posts-dir"))
                   (with-mock (mock (find-file "path-to-jekyll-root/some-posts-dir") => :found)
                              (org2jekyll-list-posts))))))

(ert-deftest test-org2jekyll-list-drafts ()
  (should (equal :found
                 (let ((org2jekyll-source-directory "path-to-org-source-root")
                       (org2jekyll-jekyll-drafts-dir "some-drafts-dir"))
                   (with-mock (mock (find-file "path-to-org-source-root/some-drafts-dir") => :found)
                              (org2jekyll-list-drafts))))))

(ert-deftest test-org2jekyll-message ()
  (should (string= "org2jekyll - this is a message"
                   (org2jekyll-message "this is a %s" "message")))
  (should (string= "org2jekyll - this is another message!"
                   (org2jekyll-message "this is %s %s!" "another" "message"))))

(ert-deftest test-org2jekyll-now ()
  (should (string= :date
                   (with-mock (mock (format-time-string "%Y-%m-%d %a %H:%M") => :date)
                              (org2jekyll-now)))))

(ert-deftest test-org2jekyll-create-draft ()
  (should (string= "#+STARTUP: showall
#+STARTUP: hidestars
#+OPTIONS: H:2 num:nil tags:nil toc:nil timestamps:t
#+LAYOUT: post
#+AUTHOR: tony
#+DATE: some date
#+TITLE: some title
#+DESCRIPTION: some description
#+TAGS: tag0, tag1
#+CATEGORIES: cat0, cat1, catn

* "
                   (progn
                     ;; clean up
                     (when (file-exists-p "/tmp/some-title.org")
                       (delete-file "/tmp/some-title.org"))
                     ;; execute draft creation
                     (save-excursion
                       (let ((org2jekyll-source-directory "/tmp")
                             (org2jekyll-jekyll-drafts-dir "")
                             (org2jekyll-blog-author "tony"))
                         (with-mock
                           (mock (org2jekyll-now)                                                        => "some date")
                           (mock (ido-completing-read "Layout: " '("post" "default") nil 'require-match) => "post")
                           (mock (org2jekyll--read-title)                                                => "some title")
                           (mock (org2jekyll--read-description)                                          => "some description")
                           (mock (org2jekyll--read-tags)                                                 => "tag0, tag1")
                           (mock (org2jekyll--read-categories)                                           => "cat0, cat1, catn")
                           (mock (org2jekyll-input-directory "")                                         => "/tmp")
                           ;; (mock (org2jekyll--draft-filename "/tmp" "some title")                     => "/tmp/some-title.org")
                           (mock (org2jekyll--draft-filename * *)                                        => "/tmp/some-title.org")
                           (mock (find-file "/tmp/some-title.org") => nil)
                           (call-interactively #'org2jekyll-create-draft))))
                     ;; read the created file
                     (with-temp-buffer
                       (insert-file-contents "/tmp/some-title.org")
                       (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest test-org2jekyll-init-current-buffer ()
  (should (string= "#+STARTUP: showall
#+STARTUP: hidestars
#+OPTIONS: H:2 num:nil tags:nil toc:nil timestamps:t
#+LAYOUT: some-layout
#+AUTHOR: dude
#+DATE: some-date
#+TITLE: some-title
#+DESCRIPTION: some-desc
#+TAGS: some-tags
#+CATEGORIES: some-cat

* some blog
* already present
"
                   (org2jekyll-tests-with-temp-buffer-and-return-content
                    "* some blog
* already present
"
                    (with-mock
                      (mock (org2jekyll--init-buffer-metadata) => '(:author "dude"
                                                                            :date "some-date"
                                                                            :layout "some-layout"
                                                                            :title "some-title"
                                                                            :description "some-desc"
                                                                            :tags "some-tags"
                                                                            :categories "some-cat"))
                      (call-interactively #'org2jekyll-init-current-buffer))))))


(ert-deftest test-org2jekyll--read-title ()
  (should (string= "some super title"
                   (with-mock
                     (mock (read-string "Title: ") => "some super title")
                     (org2jekyll--read-title))))
  (should-not (with-mock
                (mock (read-string "Title: "))
                (org2jekyll--read-title))))

(ert-deftest test-org2jekyll--read-description ()
  (should (string= "some super description"
                   (with-mock
                     (mock (read-string "Description: ") => "some super description")
                     (org2jekyll--read-description))))
  (should-not (with-mock
                (mock (read-string "Description: "))
                (org2jekyll--read-description))))

(ert-deftest test-org2jekyll--read-tags ()
  (should (string= "tag0, tag10"
                   (with-mock
                     (mock (read-string "Tags (csv): ") => "tag0, tag10")
                     (org2jekyll--read-tags))))
  (should-not (with-mock
                (mock (read-string "Tags (csv): "))
                (org2jekyll--read-tags))))

(ert-deftest test-org2jekyll--read-categories ()
  (should (string= "cat0, cat10"
                   (with-mock
                     (mock (read-string "Categories (csv): ") => "cat0, cat10")
                     (org2jekyll--read-categories))))
  (should-not (with-mock
                (mock (read-string "Categories (csv): "))
                (org2jekyll--read-categories))))

(ert-deftest test-org2jekyll--input-read ()
  (should (eq :input-done
              (with-mock
                (mock (ido-completing-read :prompt
                                           :collection
                                           nil
                                           'require-match) => :input-done)
                (org2jekyll--input-read :prompt :collection)))))

(ert-deftest test-org2jekyll--init-buffer-metadata ()
  (should (equal '(:author "dude"
                           :date :some-date
                           :layout :some-layout
                           :title :some-title
                           :description :some-desc
                           :tags :some-tags
                           :categories :some-cat)
                 (let ((org2jekyll-blog-author "dude"))
                   (with-mock
                     (mock (org2jekyll-now) => :some-date)
                     (mock (org2jekyll--input-read "Layout: " '("post" "default")) => :some-layout)
                     (mock (org2jekyll--read-title) => :some-title)
                     (mock (org2jekyll--read-description) => :some-desc)
                     (mock (org2jekyll--read-tags) => :some-tags)
                     (mock (org2jekyll--read-categories) => :some-cat)
                     (org2jekyll--init-buffer-metadata))))))

(ert-deftest test-org2jekyll-publish-web-project ()
  (should (eq 'publish-done
              (with-mock
                (mock (org-publish-project "web") => 'publish-done)
                (org2jekyll-publish-web-project)))))

(ert-deftest test-org2jekyll-publish-post ()
  (should (eq :publish-post-done
              (with-mock
                (mock (org2jekyll-read-metadata-and-execute
                       'org2jekyll--publish-post-org-file-with-metadata
                       :org-file) => :publish-post-done)
                (org2jekyll-publish-post :org-file)))))

(ert-deftest test-org2jekyll-publish-page ()
  (should (eq :publish-page-done
              (with-mock
                (mock (org2jekyll-read-metadata-and-execute
                       'org2jekyll--publish-page-org-file-with-metadata
                       :org-file) => :publish-page-done)
                (org2jekyll-publish-page :org-file)))))

(ert-deftest test-org2jekyll--bug-report ()
  (should (string= "Please:
- Describe your problem with clarity and conciceness (cf. https://www.gnu.org/software/emacs/manual/html_node/emacs/Understanding-Bug-Reporting.html)
- Explicit your installation choice (melpa, marmalade, el-get, tarball, git clone...).
- A sample of your configuration.
- Report the following message trace inside your issue.

System information:
- system-type: system-type
- locale-coding-system: locale-coding-system
- emacs-version: emacs-version
- org version: org-version
- org2jekyll version: org2jekyll-version
- org2jekyll path: /path/to/org2jekyll"

                   (let ((system-type "system-type")
                         (locale-coding-system "locale-coding-system")
                         (org2jekyll--version "org2jekyll-version"))
                     (with-mock
                       (mock (emacs-version) => "emacs-version")
                       (mock (org-version) => "org-version")
                       (mock (find-library-name "org2jekyll") => "/path/to/org2jekyll")
                       (org2jekyll--bug-report))))))

(ert-deftest test-org2jekyll-bug-report ()
  (should (equal :res
                 (with-mock
                   (mock (browse-url "https://github.com/ardumont/org2jekyll/issues/new") => :opened)
                   (mock (org2jekyll--bug-report) => :message)
                   (mock (message :message) => :res)
                   (org2jekyll-bug-report 'browse))))
  (should (equal :res
                 (with-mock
                   (mock (org2jekyll--bug-report) => :message2)
                   (mock (message :message2) => :res)
                   (org2jekyll-bug-report)))))

;;; org2jekyll-test.el ends here
