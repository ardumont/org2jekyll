;;; org2jekyll.el --- Minor mode to publish org-mode post to jekyll without specific yaml

;; Copyright (C) 2014 Antoine R. Dumont <eniotna.t AT gmail.com>

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Maintainer: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.1.1
;; Package-Requires: ((dash "2.10.0") (dash-functional "1.2.0") (s "1.9.0") (deferred "0.3.1"))
;; Keywords: org-mode jekyll blog publish
;; URL: https://github.com/ardumont/org2jekyll

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Functions to ease publishing jekyll posts from org-mode file

;; Providing you have a working `'jekyll`' and `'org-publish`'
;; This will permit you to simply export an org-mode file with the right jekyll
;; format to the right folder
;;
;; M-x org2jekyll/create-draft! create a draft with the necessary metadata
;;
;; M-x org2jekyll/publish! publish the current post (or page) to the jekyll folder
;;
;; M-x org2jekyll/publish-pages! to publish all pages
;;
;; M-x org2jekyll/publish-posts! to publish all post pages
;;
;; M-x org2jekyll-mode to activate org2jekyll minor mode
;;
;; You can customize using M-x customize-group RET org2jekyll RET
;;
;; More information on https://github.com/ardumont/org2jekyll

;;; Code:

(require 'org)
(require (if (version< emacs-version "24.4") 'org-publish 'ox-publish))

(require 'dash)
(require 's)
(require 'deferred)

(defgroup org2jekyll nil " Org2jekyll customisation group."
  :tag "org2jekyll"
  :version "0.0.3"
  :group 'org)

(defcustom org2jekyll/blog-author nil
  "Blog entry author."
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defcustom org2jekyll/source-directory nil
  "Path to the source directory."
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defcustom org2jekyll/jekyll-directory nil
  "Path to Jekyll blog."
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defcustom org2jekyll/jekyll-drafts-dir nil
  "Relative path to drafts directory."
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defcustom org2jekyll/jekyll-posts-dir nil
  "Relative path to posts directory."
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defvar org2jekyll/jekyll-post-ext ".org"
  "File extension of Jekyll posts.")

(defvar org2jekyll/jekyll-org-post-template nil
  "Default template for org2jekyll draft posts.
The `'%s`' will be replaced respectively by name, the author, the generated date, the title, the description and the categories.")

(setq org2jekyll/jekyll-org-post-template
      "#+STARTUP: showall\n#+STARTUP: hidestars\n#+OPTIONS: H:2 num:nil tags:nil toc:nil timestamps:t\n#+LAYOUT: post\n#+AUTHOR: %s\n#+DATE: %s\n#+TITLE: %s\n#+DESCRIPTION: %s\n#+CATEGORIES: %s\n\n")

(defun org2jekyll/--optional-folder (folder-source &optional folder-name)
  "Compute the folder name from a FOLDER-SOURCE and an optional FOLDER-NAME."
  (format "%s/%s" folder-source (if folder-name folder-name "")))

(defun org2jekyll/input-directory (&optional folder-name)
  "Compute the input folder from the FOLDER-NAME."
  (org2jekyll/--optional-folder org2jekyll/source-directory folder-name))

(defun org2jekyll/output-directory (&optional folder-name)
  "Compute the output folder from the optional FOLDER-NAME."
  (org2jekyll/--optional-folder org2jekyll/jekyll-directory folder-name))

(defun org2jekyll/--make-slug (s)
  "Turn a string S into a slug."
  (replace-regexp-in-string
   " " "-" (downcase
            (replace-regexp-in-string
             "[^A-Za-z0-9 ]" "" s))))

(defun org2jekyll/--yaml-escape (s)
  "Escape a string S for YAML."
  (if (or (string-match ":" s)
          (string-match "\"" s))
      (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"")
    s))

(defun org2jekyll/now! ()
  "Generate a formatted now date."
  (format-time-string "%Y-%m-%d %a %H:%M"))

(defun org2jekyll/default-headers-template (blog-layout blog-author post-date post-title post-description post-categories)
  "Compute default headers.
BLOG-LAYOUT is the layout of the post.
BLOG-AUTHOR is the author.
POST-DATE is the date of the post.
POST-TITLE is the title.
POST-DESCRIPTION is the description.
POST-CATEGORIES is the categories."
  (format org2jekyll/jekyll-org-post-template blog-layout blog-author post-date (org2jekyll/--yaml-escape post-title) post-description post-categories))

;;;###autoload
(defun org2jekyll/create-draft! ()
  "Create a new Jekyll blog post with TITLE."
  (interactive)
  "The `'%s`' will be replaced respectively by the blog entry name, the author, the generated date, the title, the description and the categories."
  (let ((author      org2jekyll/blog-author)
        (date        (org2jekyll/now!))
        (layout      (read-string "Layout (post, default, ...): "))
        (title       (read-string "Title: "))
        (description (read-string "Description: "))
        (categories  (read-string "Categories (comma separated entries): ")))
    (let ((draft-file (concat (org2jekyll/input-directory org2jekyll/jekyll-drafts-dir)
                              (org2jekyll/--make-slug title)
                              org2jekyll/jekyll-post-ext)))
      (if (file-exists-p draft-file)
          (find-file draft-file)
        (progn (find-file draft-file)
               (insert (org2jekyll/default-headers-template layout author date title description categories))
               (insert "* "))))))

;;;###autoload
(defun org2jekyll/list-posts ()
  "Lists the posts folder."
  (interactive)
  (find-file (org2jekyll/output-directory org2jekyll/jekyll-posts-dir)))

;;;###autoload
(defun org2jekyll/list-drafts ()
  "Lists the drafts folder."
  (interactive)
  (find-file (org2jekyll/output-directory org2jekyll/jekyll-drafts-dir)))

(defun org2jekyll/get-option-at-point! (opt)
  "Gets the header value of the option OPT from a buffer."
  (let* ((regexp (org-make-options-regexp (list (upcase opt) (downcase opt)))))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward regexp nil t 1)
          (match-string-no-properties 2)))))

(defun org2jekyll/get-option-from-file! (orgfile option)
  "Return the ORGFILE's OPTION."
  (with-temp-buffer
    (when (file-exists-p orgfile)
      (insert-file-contents orgfile)
      (goto-char (point-min))
      (org2jekyll/get-option-at-point! option))))

(defun org2jekyll/get-options-from-file! (orgfile options)
  "Return the ORGFILE's OPTIONS."
  (with-temp-buffer
    (when (file-exists-p orgfile)
      (insert-file-contents orgfile)
      (mapcar (lambda (option)
                (save-excursion
                  (goto-char (point-min))
                  (cons option (org2jekyll/get-option-at-point! option))))
              options))))

(defun org2jekyll/layout! (org-file)
  "Determine if the current ORG-FILE's layout.
Depends on the metadata header #+LAYOUT."
  (org2jekyll/get-option-from-file! org-file "layout"))

(defalias 'org2jekyll/article-p! 'org2jekyll/layout!)

(defvar org2jekyll/map-keys '(("title"       . "title")
                              ("categories"  . "categories")
                              ("date"        . "date")
                              ("description" . "excerpt")
                              ("author"      . "author")
                              ("layout"      . "layout"))
  "Keys to map from org headers to jekyll's headers.")

(defun org2jekyll/--org-to-yaml-metadata (org-metadata)
  "Given an ORG-METADATA map, return a yaml one with transformed data."
  (--map `(,(assoc-default (car it) org2jekyll/map-keys) . ,(cdr it)) org-metadata))

(defun org2jekyll/--convert-timestamp-to-yyyy-dd-mm (timestamp)
  "Convert org TIMESTAMP to ."
  (format-time-string "%Y-%m-%d"
                      (apply 'encode-time (org-parse-time-string timestamp))))

(defun org2jekyll/--to-yaml-header (org-metadata)
  "Given a list of ORG-METADATA, compute the yaml header string."
  (--> org-metadata
    org2jekyll/--org-to-yaml-metadata
    (--map (format "%s: %s" (car it) (cdr it)) it)
    (cons "---" it)
    (cons "#+BEGIN_HTML" it)
    (-snoc it "---")
    (-snoc it "#+END_HTML\n")
    (s-join "\n" it)))

(defun org2jekyll/--categories-csv-to-yaml (categories-csv)
  "Transform a CATEGORIES-CSV entries into a yaml entries."
  (->> categories-csv
    (concat ",")
    (s-replace ", " ",")
    (s-replace "," "\n- ")
    s-trim
    (concat "\n")))

(defun org2jekyll/--compute-ready-jekyll-file-name (date org-file)
  "Given a DATE and an ORG-FILE, compute a ready jekyll file name.
If the current path contains the `'org2jekyll/jekyll-drafts-dir`', removes it."
  (let ((temp-org-jekyll-filename  (format "%s-%s" date (file-name-nondirectory org-file)))
        (temp-org-jekyll-directory (file-name-directory org-file)))
    (->> temp-org-jekyll-filename
      (format "%s%s" temp-org-jekyll-directory)
      (replace-regexp-in-string (format "%s" org2jekyll/jekyll-drafts-dir) "")
      (replace-regexp-in-string "//" "/"))))

(defun org2jekyll/--copy-org-file-to-jekyll-org-file (date org-file yaml-headers)
  "Given DATE, ORG-FILE and YAML-HEADERS, copy content as org-jekyll ready file.
This returns the new filename path."
  (let ((jekyll-filename (org2jekyll/--compute-ready-jekyll-file-name date org-file)))
    (with-temp-file jekyll-filename ;; write temporary file updated with jekyll specifics
      (insert-file-contents org-file)
      (goto-char (point-min))
      (insert (org2jekyll/--to-yaml-header yaml-headers)))
    jekyll-filename))

(defun org2jekyll/assoc-default (key org-data default-value)
  "Given KEY, ORG-DATA and DEFAULT-VALUE, return the value associated with key.
Return DEFAULT-VALUE if not found."
  (-if-let (data (assoc-default key org-data nil default-value))
      data
    default-value))

(defvar org2jekyll/header-metadata! nil
  "The needed headers for org buffer for org2jekyll to work.")

(setq org2jekyll/header-metadata! '(("title" . 'mandatory)
                                    ("date")
                                    ("categories" . 'mandatory)
                                    ("description" . 'mandatory)
                                    ("author")
                                    ("layout" . 'mandatory)))

(require 'dash-functional)

(defun org2jekyll/check-metadata (org-metadata)
  "Check that the mandatory header metadata in ORG-METADATA are provided.
Return the error messages if any or nil if everything is alright."
  (let ((mandatory-values (funcall (-compose (lambda (l) (mapcar #'car l))
                                             (lambda (l) (-filter #'cdr l))) org2jekyll/header-metadata!)))
    (-when-let (error-messages (->> mandatory-values
                                 (--map (when (null (assoc-default it org-metadata))
                                          (format "- The %s is mandatory, please add '#+%s' at the top of your org buffer." it (upcase it))))
                                 (s-join "\n")
                                 s-trim))
      (if (string= "" error-messages) nil error-messages))))

(defun org2jekyll/read-metadata! (org-file)
  "Given an ORG-FILE, return its org metadata.
If non-mandatory values are missing, they are replaced with dummy ones.
Otherwise, display the error messages about the missing mandatory values."
  (let* ((org-metadata-list (mapcar #'car org2jekyll/header-metadata!))
         (org-metadata (org2jekyll/get-options-from-file! org-file org-metadata-list)))
    (-if-let (error-messages (org2jekyll/check-metadata org-metadata))
        (format "This org-mode file is missing mandatory header(s):\n%s\nPublication skipped!" error-messages)
      `(("layout"      . ,(-> "layout"      (org2jekyll/assoc-default org-metadata "post")))
        ("title"       . ,(-> "title"       (org2jekyll/assoc-default org-metadata "dummy-title-should-be-replaced")))
        ("date"        . ,(-> "date"        (org2jekyll/assoc-default org-metadata (org2jekyll/now!)) org2jekyll/--convert-timestamp-to-yyyy-dd-mm))
        ("categories"  . ,(-> "categories"  (org2jekyll/assoc-default org-metadata "dummy-category-should-be-replaced") org2jekyll/--categories-csv-to-yaml))
        ("author"      . ,(-> "author"      (org2jekyll/assoc-default org-metadata "")))
        ("description" . ,(-> "description" (org2jekyll/assoc-default org-metadata "")))))))

(defun org2jekyll/read-metadata-and-execute! (action-fn org-file)
  "Execute ACTION-FN function after checking metadata from the ORG-FILE."
  (let ((filename-non-dir (file-name-nondirectory org-file)))
    (if (org2jekyll/article-p! org-file)
        (let ((org-metadata (org2jekyll/read-metadata! org-file)))
          (if (stringp org-metadata)
              (org2jekyll/message org-metadata)
            (let ((page-or-post (if (org2jekyll/post-p! (assoc-default "layout" org-metadata)) "Post" "Page")))
              (funcall action-fn org-metadata org-file)
              (org2jekyll/message "%s '%s' published!" page-or-post filename-non-dir))))
      (org2jekyll/message "'%s' is not an article, publication skipped!" filename-non-dir))))

(defun org2jekyll/message (&rest args)
  "Log formatted ARGS."
  (apply 'message (format "org2jekyll - %s" (car args)) (cdr args)))

(defun org2jekyll/publish-post! (org-file)
  "Publish ORG-FILE as a post."
  (org2jekyll/read-metadata-and-execute!
   (lambda (org-metadata org-file)
     (let ((blog-project    (assoc-default "layout" org-metadata))
           (jekyll-filename (org2jekyll/--copy-org-file-to-jekyll-org-file (assoc-default "date" org-metadata) org-file org-metadata)))
       (org-publish-file jekyll-filename (assoc blog-project org-publish-project-alist))
       (delete-file jekyll-filename)))
   org-file))

(defun org2jekyll/publish-page! (org-file)
  "Publish ORG-FILE as a page."
  (org2jekyll/read-metadata-and-execute!
   (lambda (org-metadata org-file)
     (let ((blog-project (assoc-default "layout" org-metadata))
           (backup-file (format "%s.org2jekyll" org-file)))
       (copy-file org-file backup-file t t t)
       (with-temp-file org-file
         (insert-file-contents org-file)
         (goto-char (point-min))
         (insert (org2jekyll/--to-yaml-header org-metadata))
         (org-publish-file org-file (assoc blog-project org-publish-project-alist)))
       (copy-file backup-file org-file t t t)
       (delete-file backup-file)))
   org-file))

(defun org2jekyll/post-p! (layout)
  "Determine if the LAYOUT corresponds to a post."
  (string= "post" layout))

(defun org2jekyll/page-p! (layout)
  "Determine if the LAYOUT corresponds to a page."
  (string= "default" layout))

;;;###autoload
(defun org2jekyll/publish! ()
  "Publish the current org file as post or page depending on the chosen layout.
Layout `'post`' is a jekyll post.
Layout `'default`' is a page."
  (interactive)
  (lexical-let ((org-file (buffer-file-name (current-buffer))))
    (deferred:$ (deferred:call
                  (lambda ()
                    (-> "layout"
                      org2jekyll/get-option-at-point!
                      org2jekyll/post-p!
                      (if 'org2jekyll/publish-post! 'org2jekyll/publish-page!))))
      (deferred:nextc it
        (lambda (publish-fn) (funcall publish-fn org-file))))))

(defvar org2jekyll-mode-map nil "Default Bindings map for org2jekyll minor mode.")

(setq org2jekyll-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c . n") 'org2jekyll/create-draft!)
        (define-key map (kbd "C-c . p") 'org2jekyll/publish!)
        (define-key map (kbd "C-c . P") 'org2jekyll/publish-posts!)
        (define-key map (kbd "C-c . l") 'org2jekyll/list-posts)
        (define-key map (kbd "C-c . d") 'org2jekyll/list-drafts)
        map))

;;;###autoload
(defun org2jekyll/publish-posts! ()
  "Publish all the posts."
  (interactive)
  (deferred:$
    (deferred:next
      (lambda () (->> (assoc "post" org-publish-project-alist)
              org-publish-get-base-files
              (--filter (org2jekyll/post-p! (org2jekyll/article-p! it))))))
    (deferred:nextc it
      (lambda (posts)
        (mapc #'org2jekyll/publish-post! posts)))))

;;;###autoload
(defun org2jekyll/publish-pages! ()
  "Publish all the pages."
  (interactive)
  (deferred:$
    (deferred:next
      (lambda () (->> (assoc "default" org-publish-project-alist)
              org-publish-get-base-files
              (--filter (org2jekyll/page-p! (org2jekyll/article-p! it))))))
    (deferred:nextc it
      (lambda (pages)
        (mapc #'org2jekyll/publish-page! pages)))))

;;;###autoload
(define-minor-mode org2jekyll-mode
  "Toggle org2jekyll-mode mode.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:
\\{org2jekyll-mode-map}"

  :init-value nil
  :lighter " o2j"
  :group 'org2jekyll
  :keymap org2jekyll-mode-map)

(provide 'org2jekyll)
;;; org2jekyll.el ends here
