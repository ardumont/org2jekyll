;;; testing-blog-config.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Antoine R. Dumont (@ardumont)

;; Author: Antoine R. Dumont (@ardumont) <tony@yavin4>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'org)

(setq org-publish-cache nil)

(custom-set-variables
 ;; org specifics to make the tryouts reproducible here
 '(org-publish-use-timestamps-flag nil) ;; testing: no timestamp checking and always publish all files
 '(org-publish-timestamp-directory (expand-file-name "./org-timestamps/"))

 ;; org2jekyll specifics
 '(org2jekyll-jekyll-layout-page  "page")
 '(org2jekyll-jekyll-layouts     '("page" "post"))
 '(org2jekyll-blog-author        "drjekyll&mrtony")
 '(org2jekyll-source-directory   (expand-file-name "org"))
 '(org2jekyll-jekyll-directory   (expand-file-name ""))
 '(org2jekyll-jekyll-drafts-dir  "_drafts")
 '(org2jekyll-jekyll-posts-dir   "_posts/")
 '(org-publish-project-alist
   `(("page"  ;; for mostly static pages (not blog post): about-me, contacts, etc...
      :base-directory ,(org2jekyll-input-directory)
      :base-extension "org"
      :publishing-directory ,(org2jekyll-output-directory)
      :publishing-function org-html-publish-to-html
      :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
      :html-preamble t
      :recursive t
      :make-index t
      :html-extension "html"
      :body-only t)
     ("post"  ;; dynamic blog posts
      :base-directory ,(org2jekyll-input-directory)
      :base-extension "org"
      :publishing-directory ,(org2jekyll-output-directory org2jekyll-jekyll-posts-dir)
      :publishing-function org-html-publish-to-html
      :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
      :html-preamble t
      :recursive t
      :make-index t
      :html-extension "html"
      :body-only t)
     ("images"
      :base-directory ,(org2jekyll-input-directory "img")
      :base-extension "jpg\\|gif\\|png"
      :publishing-directory ,(org2jekyll-output-directory "assets/img")
      :publishing-function org-publish-attachment
      :recursive t)
     ("js"
      :base-directory ,(org2jekyll-input-directory "js")
      :base-extension "js"
      :publishing-directory ,(org2jekyll-output-directory "assets/js")
      :publishing-function org-publish-attachment
      :recursive t)
     ("css"
      :base-directory ,(org2jekyll-input-directory "css")
      :base-extension "css\\|el"
      :publishing-directory ,(org2jekyll-output-directory "assets/css")
      :publishing-function org-publish-attachment
      :recursive t)
     ("web" :components ("images" "js" "css")))))

(provide 'testing-blog-config)
;;; testing-blog-config.el ends here
