;;; org2jekyll-test.el --- Test org2jekyll
;;; Commentary:

(require 'ert)
(require 'el-mock)
(require 'org2jekyll-utilities)

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

(ert-deftest test-org2jekyll--space-separated-values-to-yaml ()
  (should (string= "\n- jabber\n- emacs\n- gtalk\n- tools\n- authentication"
                   (org2jekyll--space-separated-values-to-yaml "jabber emacs gtalk tools authentication")))
  (should (string= "\n- jabber\n- emacs\n- gtalk\n- tools\n- authentication"
                   (org2jekyll--space-separated-values-to-yaml "jabber emacs  gtalk tools  authentication")))
  (should (string= ""
                   (org2jekyll--space-separated-values-to-yaml nil)))
  (should (string= ""
                   (org2jekyll--space-separated-values-to-yaml ""))))

(ert-deftest test-org2jekyll--to-yaml-header ()
  (should (string= "---
layout: post
title: gtalk in emacs using jabber mode
date: 2013-01-13 10:10
author: Antoine R. Dumont
categories: \n- jabber\n- emacs\n- tools\n- gtalk
tags: \n- tag0\n- tag1\n- tag2
excerpt: Installing jabber and using it from emacs + authentication tips and tricks
comments: true
permalink: /posts/gtalk/
---
"
                   (org2jekyll--to-yaml-header '(("layout" . "post")
                                                 ("title" . "gtalk in emacs using jabber mode")
                                                 ("date" . "2013-01-13 10:10")
                                                 ("author" . "Antoine R. Dumont")
                                                 ("categories" . "\n- jabber\n- emacs\n- tools\n- gtalk")
                                                 ("tags"  . "\n- tag0\n- tag1\n- tag2")
                                                 ("description" . "Installing jabber and using it from emacs + authentication tips and tricks")
                                                 ("comments" . "true")
                                                 ("permalink" . "/posts/gtalk/"))))))

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
  (should (string= "/home/user/org/2012-10-10-scratch.org"
                   (let ((org2jekyll-source-directory "/home/user/org"))
                     (org2jekyll--compute-ready-jekyll-file-name "2012-10-10" "/home/tony/org/scratch.org"))))
  (should (string= "/home/dude/repo/org/2020-05-10-awesome-post.org"
                   (let ((org2jekyll-source-directory "/home/dude/repo/org"))
                     (org2jekyll--compute-ready-jekyll-file-name "2020-05-10" "/home/dude/repo/org/some/nested/path/awesome-post.org"))))
  (should (string= "/some/path/folder/2009-12-01-scratch.org"
                   (let* ((org2jekyll-source-directory "/some/path/folder")
                          (fake-drafts-folder "fake-drafts-folder")
                          (org2jekyll-jekyll-drafts-dir fake-drafts-folder))
                     (org2jekyll--compute-ready-jekyll-file-name "2009-12-01" (format "/some/path/folder/%s/scratch.org" fake-drafts-folder))))))

(ert-deftest test-org2jekyll--convert-timestamp-to-yyyy-dd-mm ()
  "Convert to simple YYYY-MM-DD date like out of either org date of iso8601 like dates"
  (should (string= "2013-04-29" (org2jekyll--convert-timestamp-to-yyyy-dd-mm "2013-04-29 lun. 00:46")))
  (should (string= "2013-04-29" (org2jekyll--convert-timestamp-to-yyyy-dd-mm "2013-04-29 00:46"))))

(ert-deftest test-org2jekyll--convert-timestamp-to-yyyy-dd-mm-hh ()
  "Convert org-mode date to iso8601 like date strings"
  (should (string= "2020-04-29 00:46"
                   (org2jekyll--convert-timestamp-to-yyyy-dd-mm-hh "2020-04-29 lun. 00:46"))))

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
         (options-key "#+OPTIONS:") (options-val "H:2 num:nil tags:t toc:nil timestamps:t")
         (layout-key "#+LAYOUT:") (layout-val "default")
         (author-key "#+AUTHOR:") (author-val "me")
         (date-key "#+DATE:") (date-val "2015-12-23 Sat 14:20")
         (title-key "#+TITLE:") (title-val "some-title")
         (description-key "#+DESCRIPTION:") (description-val "desc")
         (tags-key "#+TAGS:") (tags-val "tag0 tag1")
         (categories-key "#+CATEGORIES:") (categories-val "cat0 cat1")
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
    (should (string= "2015-12-23 14:20" (assoc-default "date" options-alist)))
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
#+OPTIONS: H:2 num:nil tags:t toc:nil timestamps:t
#+LAYOUT: some-layout
#+AUTHOR: blog-author
#+DATE: post-date
#+TITLE: post title with spaces
#+DESCRIPTION: post some description
#+TAGS: post-tag0 post-tag1
#+CATEGORIES: post-category other-category

"
                   (org2jekyll-default-headers-template "some-layout"
                                                        "blog-author"
                                                        "post-date"
                                                        "post title with spaces"
                                                        "post some description"
                                                        "post-tag0 post-tag1"
                                                        "post-category other-category"))))

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

(ert-deftest test-org2jekyll-publish-temp-file-then-cleanup ()
  "Temporary file should be published then cleaned-up"
  (should (eq :published-file
              (let ((temp-file "/tmp/temp-file"))
                (with-mock
                  (mock (copy-file :org-file temp-file 'overwrite 'keep-time 'preserve-ids 'preserve-perms) => nil)
                  (mock (org-publish-file temp-file :project-metadata))
                  (org2jekyll--publish-temp-file-then-cleanup :org-file "/tmp/temp-file" :project-metadata))
                (if (file-exists-p temp-file)
                    :something-is-wrong
                  :published-file)))))

(ert-deftest test-org2jekyll--publish-post-org-file-with-metadata ()
  (should (eq :published-post-file
              (let ((org-publish-project-alist '((:post :project)))
                    (temp-file "/tmp/temp-file"))
                (with-mock
                  (mock (org2jekyll--convert-timestamp-to-yyyy-dd-mm :date) => :date)
                  (mock (org2jekyll--compute-ready-jekyll-file-name :date :org-file) => temp-file)
                  (mock (org2jekyll--publish-temp-file-then-cleanup :org-file temp-file
                                                                    '(:post :project)) => :published-post-file)
                  (org2jekyll--publish-post-org-file-with-metadata '(("layout" . :post)
                                                                     ("date" . :date))
                                                                   :org-file))))))

(ert-deftest test-org2jekyll--publish-page-org-file-with-metadata ()
  (should (eq :published-page-file
              (let ((org-publish-project-alist '((:page :project)))
                    (org2jekyll-source-directory "/tmp/")
                    (temp-file "/tmp/filename.org2jekyll"))
                (with-mock
                  (mock (org2jekyll--publish-temp-file-then-cleanup "filename.org"
                                                                    temp-file
                                                                    '(:page :project)) => :published-page-file)
                  (org2jekyll--publish-page-org-file-with-metadata '(("layout" . :page)
                                                                     ("date" . :date)) "filename.org"))))))

(ert-deftest test-org2jekyll-post-p ()
  "With default layouts"
  (should (org2jekyll-post-p "post"))
  (should-not (org2jekyll-post-p "default")))

(ert-deftest test-org2jekyll-post-p-with-customs ()
  "With customs layouts"
  (should
   (let ((org2jekyll-jekyll-layout-post "something"))
     (org2jekyll-post-p "something")))
  (should-not
   (let ((org2jekyll-jekyll-layout-post "something"))
     (org2jekyll-post-p "default"))))

(ert-deftest test-org2jekyll-page-p ()
  "With default layouts"
  (should (org2jekyll-page-p "default"))
  (should-not (org2jekyll-page-p "post")))

(ert-deftest test-org2jekyll-page-p-with-customs ()
  "With custom layouts"
  (should
   (let ((org2jekyll-jekyll-layout-page "post"))
     (org2jekyll-page-p "post")))
  (should-not
   (let ((org2jekyll-jekyll-layout-page "else"))
     (org2jekyll-page-p "post"))))

(ert-deftest test-org2jekyll-check-metadata ()
  (let* ((temp-file "/tmp/test-org2jekyll-check-metadata")
         (startup-key "#+STARTUP:") (startup-val "hidestars")
         (options-key "#+OPTIONS:") (options-val "H:2 num:nil tags:t toc:nil timestamps:t")
         (layout-key "#+LAYOUT:") (layout-val "some-layout")
         (author-key "#+AUTHOR:") (author-val "blog-author")
         (date-key "#+DATE:") (date-val "post-date")
         (title-key "#+TITLE:") (title-val "post title with spaces")
         (description-key "#+DESCRIPTION:") (description-val "post some description")
         (tags-key "#+TAGS:") (tags-val "post-tag0 post-tag1")
         (categories-key "#+CATEGORIES:") (categories-val "post-category other-category")
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
#+OPTIONS: H:2 num:nil tags:t toc:nil timestamps:t
#+LAYOUT: post
#+AUTHOR: tony
#+DATE: some date
#+TITLE: some title
#+DESCRIPTION: some description
#+TAGS: tag0 tag1
#+CATEGORIES: cat0 cat1 catn
#+THEME: dark
#+COMMENTS: false

* "
                   (progn
                     ;; clean up
                     (when (file-exists-p "/tmp/some-title.org")
                       (delete-file "/tmp/some-title.org"))
                     ;; Define extra headers
                     (custom-set-variables
                      '(org2jekyll-default-template-entries-extra '(("theme" "dark")
                                                                    ("comments" "false"))))
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
                           (mock (org2jekyll--read-tags)                                                 => "tag0 tag1")
                           (mock (org2jekyll--read-categories)                                           => "cat0 cat1 catn")
                           (mock (org2jekyll-input-directory "")                                         => "/tmp")
                           (mock (org2jekyll--draft-filename * *)                                        => "/tmp/some-title.org")
                           (mock (find-file "/tmp/some-title.org") => nil)
                           (call-interactively #'org2jekyll-create-draft))))
                     ;; Revert extra headers (common to all tests ¯\_(ツ)_/¯)
                     (custom-set-variables
                      '(org2jekyll-default-template-entries-extra nil))
                     ;; read the created file
                     (with-temp-buffer
                       (insert-file-contents "/tmp/some-title.org")
                       (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest test-org2jekyll-init-current-buffer ()
  (should (string= "#+STARTUP: showall
#+STARTUP: hidestars
#+OPTIONS: H:2 num:nil tags:t toc:nil timestamps:t
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
  (should (string= "tag0 tag10"
                   (with-mock
                    (mock (read-string "Tags (space separated values): ") => "tag0 tag10")
                    (org2jekyll--read-tags))))
  (should-not (with-mock
               (mock (read-string "Tags (space separated values): "))
               (org2jekyll--read-tags))))

(ert-deftest test-org2jekyll--read-categories ()
  (should (string= "cat0 cat10"
                   (with-mock
                    (mock (read-string "Categories (space separated values): ") => "cat0 cat10")
                    (org2jekyll--read-categories))))
  (should-not (with-mock
               (mock (read-string "Categories (space separated values): "))
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

(ert-deftest test-org2jekyll-version ()
  (should (string= "version-org2jekyll"
                   (let ((org2jekyll--version "version-org2jekyll"))
                     (call-interactively 'org2jekyll-version)))))

(ert-deftest test-org2jekyll--without-option-p-no-options-passed ()
  (should (with-temp-buffer
            (insert "#+OPTIONS: H:2 num:nil toc:nil timestamps:t\n")
            (org2jekyll--without-option-p "toc")))
  ;; mentioned to t, activated
  (should-not (with-temp-buffer
                (insert "#+OPTIONS: H:2 num:nil toc:t timestamps:t\n")
                (org2jekyll--without-option-p "toc")))
  ;; local option not present, activated
  (should-not (with-temp-buffer
                (insert "#+OPTIONS: H:2 num:nil timestamps:t\n")
                (org2jekyll--without-option-p "toc")))
  ;; no options at all then it's activated
  (should-not (with-temp-buffer
                (org2jekyll--without-option-p "toc"))))

(ert-deftest test-org2jekyll--without-option-p-options-passed ()
  ;; Option parsing should be detected appropriately
  (should (with-temp-buffer
            (insert "#+OPTIONS: H:2 num:nil timestamps:t\n")
            (org2jekyll--without-option-p "num" (org2jekyll-get-options-from-buffer))))

  (should-not (with-temp-buffer
                (insert "#+OPTIONS: H:2 toc:t timestamps:t\n")
                (org2jekyll--without-option-p "num" (org2jekyll-get-options-from-buffer))))

  (should-not (with-temp-buffer
                (insert "#+OPTIONS: H:2 num:t timestamps:t\n")
                (org2jekyll--without-option-p "num" (org2jekyll-get-options-from-buffer)))))

(ert-deftest test-org2jekyll--with-tags-p ()
  ;; tags option parsing should be detected appropriately
  (should-not (with-temp-buffer
                (insert "#+OPTIONS: H:2 num:nil tags:nil timestamps:t\n")
                (org2jekyll--with-tags-p (org2jekyll-get-options-from-buffer))))

  (should (with-temp-buffer
            (insert "#+OPTIONS: H:2 num:nil timestamps:t\n")
            (org2jekyll--with-tags-p (org2jekyll-get-options-from-buffer))))

  (should (with-temp-buffer
            (insert "#+OPTIONS: H:2 num:t tags:t timestamps:t\n")
            (org2jekyll--with-tags-p (org2jekyll-get-options-from-buffer)))))

(ert-deftest test-org2jekyll--header-entry ()
  (should (string= "#+STARTUP: indent"
                   (org2jekyll--header-entry '("startup" "indent"))))
  (should (string= "#+AUTHOR: %s"
                   (org2jekyll--header-entry '("author")))))

(ert-deftest test-org2jekyll--header-entry ()
  (should (string= "#+STARTUP: showall
#+STARTUP: noindent
#+OPTIONS: H:2 num:nil tags:t toc:nil timestamps:t
#+LAYOUT: %s
#+AUTHOR: %s

"
                   (org2jekyll--inline-headers '(("startup" "showall")
                                                 ("startup" "noindent")
                                                 ("options" "H:2 num:nil tags:t toc:nil timestamps:t")
                                                 ("layout")
                                                 ("author"))))))

(ert-deftest test-org2jekyll--symbol-to-string ()
  ;; :symbol should be converted to string
  (should (string= "source"
                   (org2jekyll--symbol-to-string :source)))
  ;; if the symbol is a string already, it's only the first character which is
  ;; removed
  (should (string= "source"
                   (org2jekyll--symbol-to-string ":source")))
  ;; careful for some other symbols
  (should (string= "il"
                   (org2jekyll--symbol-to-string nil))))

(ert-deftest test-org2jekyll--plist-to-alist ()
  ;; convert from plist to alist should be fine
  (should (equal
           '(("date" . "2015-09-06 12:59")
             ("author" . "ardumont")
             ("startup" . "hidestars")
             ("options" . "H:2 num:t tags:t toc:t timestamps:t")
             ("layout" . "post")
             ("title" . "org-trello debug tools")
             ("description" . "org-trello debugging tools")
             ("tags" . "\n- tools\n- org-trello\n- debug")
             ("categories" . "\n- tools\n- org-trello\n- debug")
             ("permalink" . "/debug/"))
           (org2jekyll--plist-to-alist '(:date "2015-09-06 12:59"
                                               :author "ardumont"
                                               :startup "hidestars"
                                               :options "H:2 num:t tags:t toc:t timestamps:t"
                                               :layout "post"
                                               :title "org-trello debug tools"
                                               :description "org-trello debugging tools"
                                               :tags "\n- tools\n- org-trello\n- debug"
                                               :categories "\n- tools\n- org-trello\n- debug"
                                               :permalink "/debug/")))))

(ert-deftest test-org2jekyll-install-yaml-headers ()
  ;; original-file css should not be modified
  (should-not (org2jekyll-install-yaml-headers "something.css" "something.css"))
  ;; puslished file as no html should not be published
  (should-not (org2jekyll-install-yaml-headers "something.org" "something.else"))
  ;; org file should be modified with extra yaml headers

  (should (string=
           "---
date: 2020-05-21 16:58
author: dude
layout: post
title: some-title
excerpt: some-desc
tags: \n- some-tags
categories: \n- some-cat
---
"
           (let ((original-file "/tmp/awesome.org")
                 (published-file "/tmp/awesome.html"))
             (with-temp-file original-file
               (insert
                "#+STARTUP: showall
#+STARTUP: hidestars
#+OPTIONS: H:2 num:nil tags:t toc:nil timestamps:t
#+LAYOUT: post
#+AUTHOR: dude
#+DATE: 2020-05-21 Thu 16:58
#+TITLE: some-title
#+DESCRIPTION: some-desc
#+TAGS: some-tags
#+CATEGORIES: some-cat

Awesome post
"))
             ;; empty file which simulates a published article
             (with-temp-file published-file (insert ""))
             ;; install yaml headers on the published file
             (org2jekyll-install-yaml-headers original-file published-file)
             ;; checking the yaml header (there are only those)x
             (with-temp-buffer
               (insert-file-contents published-file)
               (buffer-substring-no-properties (point-min) (point-max))))))

  (should (string=
           "---
date: 2020-05-21 16:58
author: guy
layout: page
title: this is a page
excerpt: description
tags: \n- tag0\n- tag1
categories: \n- cat0
---
"
           (let ((original-file "/tmp/page.org2jekyll")
                 (published-file "/tmp/page.html"))
             (with-temp-file original-file
               (insert
                "#+STARTUP: hidestars showall
#+LAYOUT: page
#+AUTHOR: guy
#+DATE: 2020-05-21 Thu 16:58
#+TITLE: this is a page
#+DESCRIPTION: description
#+TAGS: tag0 tag1
#+CATEGORIES: cat0

Awesome page
"))
             ;; empty file which simulates a published article
             (with-temp-file published-file (insert ""))
             ;; install yaml headers on the published file
             (org2jekyll-install-yaml-headers original-file published-file)
             ;; checking the yaml header (there are only those)x
             (with-temp-buffer
               (insert-file-contents published-file)
               (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest test-org2jekyll-publish ()
  (should (string= "org2jekyll - published post!"
                   (org2jekyll-tests-with-temp-buffer
                    "#+STARTUP: showall
#+STARTUP: hidestars
#+OPTIONS: H:2 num:nil tags:t toc:nil timestamps:t
#+LAYOUT: post
#+AUTHOR: dude
#+DATE: 2020-05-21 Thu 16:58
#+TITLE: some-title
#+DESCRIPTION: some-desc
#+TAGS: some-tags
#+CATEGORIES: some-cat

Awesome post
"
                    (with-mock
                      (mock (org2jekyll-publish-post nil) => "published post!")
                      (mock (org2jekyll-publish-web-project) => 'done)
                      (call-interactively 'org2jekyll-publish)))))

  (should (string= "org2jekyll - published page!"
                   (org2jekyll-tests-with-temp-buffer
                    "#+STARTUP: showall
#+STARTUP: hidestars
#+OPTIONS: H:2 num:nil tags:t toc:nil timestamps:t
#+LAYOUT: page
#+AUTHOR: dude
#+DATE: 2020-05-21 Thu 16:58
#+TITLE: some-title
#+DESCRIPTION: some-desc
#+TAGS: some-tags
#+CATEGORIES: some-cat

Awesome post
"
                    (with-mock
                      (mock (org2jekyll-publish-page nil) => "published page!")
                      (mock (org2jekyll-publish-web-project) => 'done)
                      (call-interactively 'org2jekyll-publish))))))

(ert-deftest test-org2jekyll-publish-posts ()
  (should (equal '("post.org")
                 (let ((org2jekyll-jekyll-layout-post "post")
                       (org2jekyll-jekyll-layout-page "page")
                       (org-publish-project-alist '(("post" "something"))))
                   (with-mock
                     (mock (org-publish-get-base-files '("post" "something"))
                           => '("post.org"))
                     (mock (org2jekyll-article-p "post.org") => "post")
                     (mock (org2jekyll-publish-post "post.org") => "post.org published!")
                     (call-interactively 'org2jekyll-publish-posts)))))
  (should-not (let ((org2jekyll-jekyll-layout-post "post")
                    (org2jekyll-jekyll-layout-page "page")
                    (org-publish-project-alist '(("post" "something"))))
                (with-mock
                  (mock (org-publish-get-base-files '("post" "something"))
                        => '("page.org"))
                  (mock (org2jekyll-article-p "page.org") => "page")
                  (call-interactively 'org2jekyll-publish-posts)))))

(ert-deftest test-org2jekyll-publish-pages ()
  (should-not
   (let ((org2jekyll-jekyll-layout-post "post")
         (org2jekyll-jekyll-layout-page "page")
         (org-publish-project-alist '(("page" "something-else"))))
     (with-mock
       (mock (org-publish-get-base-files '("page" "something-else"))
             => '("post.org"))
       (mock (org2jekyll-article-p "post.org") => "post")
       (call-interactively 'org2jekyll-publish-pages))))
  (should
   (equal '("page.org")
          (let ((org2jekyll-jekyll-layout-post "post")
                (org2jekyll-jekyll-layout-page "page")
                (org-publish-project-alist '(("page" "something-else"))))
            (with-mock
              (mock (org-publish-get-base-files '("page" "something-else"))
                    => '("page.org"))
              (mock (org2jekyll-article-p "page.org") => "page")
              (mock (org2jekyll-publish-page "page.org") => "page.org published!")
              (call-interactively 'org2jekyll-publish-pages))))))

;;; org2jekyll-test.el ends here
