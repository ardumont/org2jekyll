;;; org2jekyll.el --- Minor mode to publish org-mode post to jekyll without specific yaml

;; Copyright (C) 2014-2020 Antoine R. Dumont (@ardumont)

;; Author: Antoine R. Dumont (@ardumont) <antoine.romain.dumont@gmail.com>
;; Maintainer: Antoine R. Dumont (@ardumont) <antoine.romain.dumont@gmail.com>
;; Version: 0.2.7
;; Package-Requires: ((dash "2.18.0") (s "1.9.0"))
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
;; M-x org2jekyll-create-draft create a draft with the necessary metadata
;;
;; M-x org2jekyll-publish publish the current post (or page) to the jekyll folder
;;
;; M-x org2jekyll-publish-pages to publish all pages (layout 'default')
;;
;; M-x org2jekyll-publish-posts to publish all post pages (layout 'post')
;;
;; M-x org2jekyll-mode to activate org2jekyll's minor mode
;;
;; You can customize using M-x customize-group RET org2jekyll RET
;;
;; More information on https://github.com/ardumont/org2jekyll

;;; Code:

(require 'org)
(require (if (version< emacs-version "24.4") 'org-publish 'ox-publish))

(require 'dash)
(require 's)
(require 'ido)

(defconst org2jekyll--version "0.2.7" "Current org2jekyll version installed.")

(defgroup org2jekyll nil "Publish org-mode posts to jekyll"
  :tag "org2jekyll"
  :version "0.0.3"
  :group 'org)

(defcustom org2jekyll-blog-author nil
  "Blog entry author."
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defcustom org2jekyll-source-directory nil
  "Path to the source directory."
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defcustom org2jekyll-jekyll-directory nil
  "Path to Jekyll blog."
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defcustom org2jekyll-jekyll-drafts-dir nil
  "Path to drafts directory relative to `org2jekyll-jekyll-directory`."
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defcustom org2jekyll-jekyll-posts-dir nil
  "Path to posts directory relative to `org2jekyll-jekyll-directory`."
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defcustom org2jekyll-jekyll-layouts '("post" "default")
  "Possible layouts, by default either a post or a page"
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defcustom org2jekyll-jekyll-layout-post "post"
  "Article blog post layout"
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defcustom org2jekyll-jekyll-layout-page "default"
  "Article page layout, mostly intended as static pages (e.g about, contacts, etc...)"
  :type 'string
  :require 'org2jekyll
  :group 'org2jekyll)

(defvar org2jekyll-default-template-entries
  '(("startup" "showall")
    ("startup" "hidestars")
    ("options" "H:2 num:nil tags:t toc:nil timestamps:t")
    ("layout")
    ("author")
    ("date")
    ("title")
    ("description")
    ("tags")
    ("categories"))
  "Default list of tuple '(<header-name> <default-value>).
Optionally a tuple could be in the form '(<header-name>).
Its values are initialized according to the values defined in version <= 0.2.2.")

(defun org2jekyll--header-entry (header-entry)
  "Given a HEADER-ENTRY, format string as org-mode header."
  (let ((header-name  (-> header-entry car s-upcase))
        (header-value (-if-let (val (cadr header-entry)) val "%s")))
    (format "#+%s: %s" header-name header-value)))

(defun org2jekyll--inline-headers (tuple-entries)
  "Given TUPLE-ENTRIES, format as org-mode headers."
  (->> tuple-entries
       (mapcar 'org2jekyll--header-entry)
       (s-join "\n")
       (format "%s\n\n")))

(defcustom org2jekyll-default-template-entries-extra nil
  "Allow users to define extra template headers entries."
  :type 'alist)

(defvar org2jekyll-jekyll-post-ext ".org"
  "File extension of Jekyll posts.")

(defun org2jekyll--optional-folder (folder-source &optional folder-name)
  "Compute the folder name from a FOLDER-SOURCE and an optional FOLDER-NAME."
  (format "%s/%s" folder-source (if folder-name folder-name "")))

;;;###autoload
(defun org2jekyll-input-directory (&optional folder-name)
  "Compute the input folder from the FOLDER-NAME."
  (org2jekyll--optional-folder org2jekyll-source-directory folder-name))

;;;###autoload
(defun org2jekyll-output-directory (&optional folder-name)
  "Compute the output folder from the optional FOLDER-NAME."
  (org2jekyll--optional-folder org2jekyll-jekyll-directory folder-name))

(defun org2jekyll--make-slug (s)
  "Turn a string S into a slug."
  (->> s
       (replace-regexp-in-string "[\]\[(){}!#$~^\\]" "")
       downcase
       (replace-regexp-in-string " " "-")))

(defun org2jekyll--yaml-escape (s)
  "Escape a string S for YAML."
  (if (or (string-match ":" s)
          (string-match "\"" s))
      (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"")
    s))

(defun org2jekyll-now ()
  "Generate a formatted now date."
  (format-time-string "%Y-%m-%d %a %H:%M"))

(defun org2jekyll-default-headers-template (headers)
  "Return an org metadata string with entries in HEADERS."
  (mapconcat #'org2jekyll--header-entry headers "\n"))

(defun org2jekyll--draft-filename (draft-dir title)
  "Compute the draft's filename from the DRAFT-DIR and TITLE."
  (concat draft-dir (org2jekyll--make-slug title) org2jekyll-jekyll-post-ext))

(defun org2jekyll--read-title ()
  "Read the title."
  (read-string "Title: "))

(defun org2jekyll--read-description ()
  "Read the description."
  (read-string "Description: "))

(defun org2jekyll--read-tags ()
  "Read the tags."
  (read-string "Tags (space separated values): "))

(defun org2jekyll--read-categories ()
  "Read the categories."
  (read-string "Categories (space separated values): "))

(defun org2jekyll--input-read (prompt collection)
  "Input PROMPT with possibilities limited to COLLECTION."
  (ido-completing-read prompt
                       collection
                       nil
                       'require-match))

(defun org2jekyll--init-buffer-metadata (&optional ignore-plist)
  "Return an plist holding buffer metadata information collected from the user.
Any non-nil property in IGNORE-PLIST will not be collected from the user, and
will instead have its value omitted in the returned plist."
  (-concat (unless (plist-get ignore-plist :author)
	     (list :author org2jekyll-blog-author))
	   (unless (plist-get ignore-plist :date)
	     (list :date (org2jekyll-now)))
	   (unless (plist-get ignore-plist :layout)
	     (list :layout (org2jekyll--input-read "Layout: " org2jekyll-jekyll-layouts)))
	   (unless (plist-get ignore-plist :title)
	     (list :title (org2jekyll--read-title)))
	   (unless (plist-get ignore-plist :description)
	     (list :description (org2jekyll--read-description)))
	   (unless (plist-get ignore-plist :tags)
	     (list :tags (org2jekyll--read-tags)))
	   (unless (plist-get ignore-plist :categories)
	     (list :categories (org2jekyll--read-categories)))))

(defun org2jekyll--get-template-entries (&optional decided-options-alist)
  "Return the contents of org2jekyll-default-template-entries and org2jekyll-default-template-entries-extra replaced by entries is DECIDED-OPTIONS-ALIST."
  (let ((-compare-fn #'string=)
	(decided-options (-map #'car decided-options-alist)))
(--map-when (and (not (cdr it)) (-contains? decided-options (car it)))
	    (-first (lambda (option) (string= (car it) (car option)))
		    decided-options-alist)
	    (-concat org2jekyll-default-template-entries
		     org2jekyll-default-template-entries-extra))))

(defun org2jekyll--get-filtered-entries (entries ignore-list)
  "Return a list from ENTRIES whose car is not present in IGNORE-LIST."
  (let ((-compare-fn #'string=))
    (--filter (not (-contains? ignore-list (car it))) entries)))

;;;###autoload
(defun org2jekyll-init-current-buffer ()
  "Given an existing buffer, add the needed metadata to make it a post or page."
  (interactive)
  (let* ((existing-options-plist (org2jekyll-get-options-from-buffer))
	 (existing-options-alist (org2jekyll--plist-to-alist existing-options-plist))
	 (metadata (org2jekyll--plist-to-alist
		    (org2jekyll--init-buffer-metadata existing-options-plist)))
	 ;; Drop options known to already be in the buffer
	 (ignore-list (-map #'car existing-options-alist))
	 (add-to-file-options (org2jekyll--get-filtered-entries
			       (org2jekyll--get-template-entries metadata) ignore-list))
	 (add-to-file-tuples (org2jekyll--alist-to-tuples add-to-file-options)))
    (save-excursion
      (with-current-buffer (buffer-name)
        (goto-char (point-min))
	(let* ((no-options (length existing-options-alist))
	       (non-metadata-present-p (> (count-lines (point-min) (point-max))
					  no-options)))
	  ;; Ensure we insert after any existing options
	  (forward-line (length existing-options-alist))
	  ;; If the buffer ends at the end of existing options, insert a new line
	  (when (and (> no-options 0) (not (char-equal ?\n (char-before))))
	    (insert "\n"))
	  (insert (org2jekyll-default-headers-template add-to-file-tuples) "\n")
	  ;; Create line between metadata and non-metadata if non-metadata exists
	  (when (and (= no-options 0) non-metadata-present-p) (insert "\n")))))))

;;;###autoload
(defun org2jekyll-create-draft ()
  "Prompt the user for org metadata and then create a new Jekyll blog post.
The specified title will be used as the name of the file."
  (interactive)
  (let* ((metadata (org2jekyll--init-buffer-metadata))
	 (metadata-alist (org2jekyll--plist-to-alist metadata))
	 (title (plist-get metadata :title))
         (draft-file  (org2jekyll--draft-filename
                       (org2jekyll-input-directory org2jekyll-jekyll-drafts-dir)
                       title))
	 (add-to-file-options (org2jekyll--get-template-entries metadata-alist))
	 (add-to-file-tuples (org2jekyll--alist-to-tuples add-to-file-options)))
    (unless (file-exists-p draft-file)
      (with-temp-file draft-file
        (insert (org2jekyll-default-headers-template add-to-file-tuples) "\n\n")
        (insert "* ")))
    (find-file draft-file)))

(defun org2jekyll--list-dir (dir)
  "List the content of DIR."
  (find-file dir))

;;;###autoload
(defun org2jekyll-list-posts ()
  "Lists the posts folder."
  (interactive)
  (org2jekyll--list-dir
   (org2jekyll-output-directory org2jekyll-jekyll-posts-dir)))

;;;###autoload
(defun org2jekyll-list-drafts ()
  "List the drafts folder."
  (interactive)
  (org2jekyll--list-dir
   (org2jekyll-input-directory org2jekyll-jekyll-drafts-dir)))

(defun org2jekyll-get-options-from-buffer ()
  "Return special lines at the beginning of current buffer."
  (let ((special-line-regex "^#\\+\\(.+\\):[ \t]+\\(.*\\)$")
        (get-current-line (lambda ()
                            (buffer-substring-no-properties (line-beginning-position)
                                                            (line-end-position))))
        (options-plist))
    (save-excursion
      (goto-char (point-min))
      (catch 'break
        (while (string-match special-line-regex (funcall get-current-line))
          (let ((current-line (funcall get-current-line)))
            (unless (s-blank-str-p (match-string 2 current-line))
              (setq options-plist (plist-put options-plist
                                             (->> current-line
                                               (match-string 1)
                                               downcase
                                               (concat ":")
                                               intern)
                                             (match-string 2 current-line)))))
          (unless (= 0 (forward-line))
            (throw 'break nil))))
      options-plist)))

(defun org2jekyll--without-option-p (option &optional options)
  "Determine if OPTION needs to be deactivated amongst options."
  ;; FIXME: Find the proper org implementation call ¯\_(ツ)_/¯
  (let ((properties (-> (if options options (org2jekyll-get-options-from-buffer))
                        (plist-get :options))))
    (when properties
      (let ((off-option (format "%s:nil" option)))
        (->> properties
             (s-split " ")
             (--filter (string= off-option it)))))))

(defun org2jekyll--with-tags-p (options)
  "Determine, from OPTIONS if we need to export in yaml the tags options"
  (-> "tags"
      (org2jekyll--without-option-p options)
      not))

(defun org2jekyll-get-options-from-file (orgfile)
  "Return special lines at the beginning of ORGFILE."
  (with-temp-buffer
    (when (file-exists-p orgfile)
      (insert-file-contents orgfile)
      (org2jekyll-get-options-from-buffer))))

(defun org2jekyll-article-p (org-file)
  "Determine if the current ORG-FILE's layout.
Depends on the metadata header #+LAYOUT."
  (plist-get (org2jekyll-get-options-from-file org-file) :layout))

(defun org2jekyll--org-to-jekyll-metadata (org-metadata)
  "Given an ORG-METADATA map, translate Org keywords to Jekyll keywords."
  (let ((org2jekyll-map-keys '(("description" . "excerpt"))))
    (--map (-if-let (jekyll-car (assoc-default (car it) org2jekyll-map-keys))
               (cons jekyll-car (cdr it))
             it)
           org-metadata)))

(defun org2jekyll--convert-timestamp-to-yyyy-dd-mm-hh (timestamp)
  "Convert org TIMESTAMP to YYYY-MM-DD HH:MM. For yaml header purposes."
  (format-time-string "%Y-%m-%d %H:%M"
                      (apply 'encode-time (org-parse-time-string timestamp))))

(defun org2jekyll--convert-timestamp-to-yyyy-dd-mm (timestamp)
  "Convert org TIMESTAMP to to YYYY-MM-DD. For filename renaming purposes."
  (format-time-string "%Y-%m-%d"
                      (apply 'encode-time (org-parse-time-string timestamp))))

(defun org2jekyll--to-yaml-header (org-metadata)
  "Given a list of ORG-METADATA, compute the yaml header string."
  (--> (org2jekyll--org-to-jekyll-metadata org-metadata)
       (--map (format "%s: %s" (car it) (cdr it)) it)
       (cons "---" it)
       (-snoc it "---\n")
       (s-join "\n" it)))

(defun org2jekyll--space-separated-values-to-yaml (str)
  "Transform a STR of space separated values entries into yaml entries."
  (->> (if str str "")
       (s-split " ")
       (--filter (unless (equal it "") it))
       (--map (format  "- %s" it))
       (cons "")
       (s-join "\n")))

(defun org2jekyll--compute-ready-jekyll-file-name (date org-file)
  "Given a DATE and an ORG-FILE, compute a ready jekyll file name.
If the current path contains the `'org2jekyll-jekyll-drafts-dir`', removes it."
  (let* ((temp-org-jekyll-filename (format "%s-%s" date
                                           (file-name-nondirectory org-file))))
    (->> temp-org-jekyll-filename
         (format "%s/%s" org2jekyll-source-directory)
         (replace-regexp-in-string (format "%s" org2jekyll-jekyll-drafts-dir) "")
         (replace-regexp-in-string "//" "/"))))

(defconst org2jekyll-required-org-header-alist '((:title       . 'required)
                                                 (:date)
                                                 (:categories  . 'required)
                                                 (:tags)
                                                 (:description . 'required)
                                                 (:author)
                                                 (:layout      . 'required))
  "Map of required org headers for jekyll to accept rendering.")

(defun org2jekyll-check-metadata (org-metadata)
  "Check that all required headers in ORG-METADATA are provided.
Return error messages for any required headers that are missing,
and nil if no problems are found."
  (let ((required-options (funcall (-compose (lambda (l) (mapcar #'car l))
                                             (lambda (l) (-filter #'cdr l)))
                                   org2jekyll-required-org-header-alist)))
    (-when-let (error-messages
                (->> required-options
                     (--map (unless (plist-member org-metadata it)
                              (format (concat "- The %s is required, please add "
                                              "'#+%s' at the top of your org buffer.")
                                      (substring (symbol-name it) 1 nil)
                                      (upcase (substring (symbol-name it) 1 nil)))))
                     (s-join "\n")
                     s-trim))
      (if (string= "" error-messages) nil error-messages))))

(defun org2jekyll--symbol-to-string (symbol)
  "Make a string out of a SYMBOL.
symbol is of the form ':<name>'"
  (let ((s (if (stringp symbol) symbol (symbol-name symbol))))
    (substring s 1 nil)))

(defun org2jekyll--plist-to-alist (plist)
  "Make an alist out of a PLIST."
  (--map
   (let ((key (-> it
                  car
                  org2jekyll--symbol-to-string))
         (value (cadr it)))
     `(,key . ,value))
   (-partition 2 plist)))

(defun org2jekyll--alist-to-tuples (alist)
  "Return a list of tuples with values from ALIST.
Any values for which -cons-pair? returns nil are left unchanged."
  (--map (cond
	  ((and (-cons-pair? it) (cdr it)) (list (car it) (cdr it)))
	  (t it))
	 alist))

(defun org2jekyll-remove-org-only-options (yaml-alist)
  "Filter out org options with no Jekyll meaning from YAML-ALIST."
  (let* ((required-options (--map (-> it car org2jekyll--symbol-to-string)
                                  org2jekyll-required-org-header-alist))
         (org-options (--map (downcase (substring it 0 -1))
                             org-options-keywords))
         (org-only-options (--filter (not (member it required-options))
                                     org-options))
         (jekyll-options (--filter (not (member (car it) org-only-options))
                                   yaml-alist)))
    jekyll-options))

(defun org2jekyll-read-metadata (org-file)
  "Given an ORG-FILE, return its org metadata.
It can display an error message about missing required values."
  (let* ((buffer-metadata (org2jekyll-get-options-from-file org-file)))
    (-if-let (error-messages (org2jekyll-check-metadata buffer-metadata))
        (format "This org-mode file is missing required header(s):
%s
Publication skipped" error-messages)
      (let* ((org-defaults `(:date ,(org2jekyll-now) :author ""))
             (merged-metadata (org-combine-plists org-defaults buffer-metadata))
             (categories (org2jekyll--space-separated-values-to-yaml
                          (plist-get merged-metadata :categories)))
             (tags (if (org2jekyll--with-tags-p buffer-metadata)
                       (org2jekyll--space-separated-values-to-yaml
                        (plist-get merged-metadata :tags))
                     ""))
             (date (org2jekyll--convert-timestamp-to-yyyy-dd-mm-hh
                    (plist-get merged-metadata :date)))
             (yaml-metadata (-> merged-metadata
                                (plist-put :categories categories)
                                (plist-put :tags tags)
                                (plist-put :date date)))
             (yaml-alist (org2jekyll--plist-to-alist yaml-metadata)))
        (org2jekyll-remove-org-only-options yaml-alist)))))

(defun org2jekyll-read-metadata-and-execute (action-fn org-file)
  "Execute ACTION-FN function after checking metadata from the ORG-FILE."
  (let ((filename-non-dir (file-name-nondirectory org-file)))
    (if (org2jekyll-article-p org-file)
        (let ((org-metadata (org2jekyll-read-metadata org-file)))
          (if (stringp org-metadata)
              (org2jekyll-message org-metadata)
            (let ((page-or-post (if (org2jekyll-post-p
                                     (assoc-default "layout" org-metadata))
                                    "Post"
                                  "Page")))
              (funcall action-fn org-metadata org-file)
              (format "%s '%s' published!" page-or-post filename-non-dir))))
      (format "'%s' is not an article, publication skipped!" filename-non-dir))))

(defun org2jekyll-message (&rest args)
  "Log formatted ARGS."
  (apply 'message (format "org2jekyll - %s" (car args)) (cdr args)))

(defun org2jekyll--publish-temp-file-then-cleanup (org-file temp-file project)
  "Publish ORG-FILE using TEMP-FILE (with yaml header) using PROJECT metadata."
  (copy-file org-file temp-file 'overwrite 'keep-time 'preserve-ids 'preserve-perms)
  (with-temp-file temp-file
    ;; activate org2jekyll-mode to rely on its hook to work properly
    (org2jekyll-mode)
    (org-publish-file temp-file project))
  ;; the org2jekyll installed hook should have kicked-in already, if it remains
  ;; dangling temporary file, just delete it
  (when (file-exists-p temp-file)
    (delete-file temp-file)))

(defun org2jekyll--publish-post-org-file-with-metadata (org-metadata org-file)
  "Publish as post with ORG-METADATA the ORG-FILE."
  (let* ((project      (-> "layout"
                           (assoc-default org-metadata)  ;; layout is the blog-project
                           (assoc org-publish-project-alist)))
         (file-date    (-> (assoc-default "date" org-metadata)
                           org2jekyll--convert-timestamp-to-yyyy-dd-mm))
         (temp-file    (org2jekyll--compute-ready-jekyll-file-name file-date org-file)))
    (org2jekyll--publish-temp-file-then-cleanup org-file temp-file project)))

(defun org2jekyll-publish-post (org-file)
  "Publish ORG-FILE as a post."
  (org2jekyll-read-metadata-and-execute
   'org2jekyll--publish-post-org-file-with-metadata
   org-file))

(defun org2jekyll-install-yaml-headers (original-file published-file)
  "Read ORIGINAL-FILE metadata and install yaml header to PUBLISHED-FILE.
Then delete the original-file which is intended as a temporary file.
Only for org-mode file, for other files, it's a noop.
This function is intended to be used as org-publish hook function."
  (let ((original-file-ext (file-name-extension original-file))
        (published-file-ext (file-name-extension published-file)))
    ;; original-file is the temporary file generated which will be edited with
    ;; jekyll's yaml headers

    ;; careful about extensions: "post" -> org ; page -> org2jekyll
    ;; other stuff are considered neither, so it's a noop
    (when (and (or (string= "org" original-file-ext) (string= "org2jekyll" original-file-ext))
               (string= "html" published-file-ext))
      (let ((yaml-headers (-> original-file
                              org2jekyll-read-metadata
                              org2jekyll--to-yaml-header)))
        (with-temp-file published-file
          (insert-file-contents published-file)
          (goto-char (point-min))
          (insert yaml-headers))
        (delete-file original-file)))))

(defun org2jekyll--publish-page-org-file-with-metadata (org-metadata org-file)
  "Publish as page with ORG-METADATA the ORG-FILE."
  (let* ((project      (-> "layout"
                           (assoc-default org-metadata)  ;; layout is the blog-project
                           (assoc org-publish-project-alist)))
         (filename     (file-name-nondirectory org-file))
         (ext          (file-name-extension filename))
         (temp-file    (format "%s/%sorg2jekyll"
                               (s-chop-suffix "/" org2jekyll-source-directory)
                               (s-chop-suffix ext filename))))
    (org2jekyll--publish-temp-file-then-cleanup org-file temp-file project)))

(defun org2jekyll-publish-page (org-file)
  "Publish ORG-FILE as a page."
  (org2jekyll-read-metadata-and-execute
   'org2jekyll--publish-page-org-file-with-metadata
   org-file))

(defun org2jekyll-post-p (layout)
  "Determine if the LAYOUT corresponds to a post."
  (string= org2jekyll-jekyll-layout-post layout))

(defun org2jekyll-page-p (layout)
  "Determine if the LAYOUT corresponds to a page."
  (string= org2jekyll-jekyll-layout-page layout))

(defun org2jekyll-publish-web-project ()
  "Publish the 'web' project."
  (let ((result (org-publish-project "web")))
    (org2jekyll-message "Publish `'web`' project (images, css, js, etc...) done.")
    result))

;;;###autoload
(defun org2jekyll-publish ()
  "Publish the current org file as post or page depending on the chosen layout.
Layout `'post`' is a jekyll post.
Layout `'default`' is a page (depending on the user customs)."
  (interactive)
  (let* ((buffer (current-buffer))
         (org-file (buffer-file-name buffer))
         (org-options (with-current-buffer buffer (org2jekyll-get-options-from-buffer)))
         (publish-fn (-> (plist-get org-options :layout)
                         org2jekyll-post-p
                         (if 'org2jekyll-publish-post
                             'org2jekyll-publish-page)))
         (final-message (funcall publish-fn org-file)))
    (progn
      (org2jekyll-publish-web-project)
      (org2jekyll-message final-message))))

(defvar org2jekyll-mode-map nil "Default Bindings map for org2jekyll mode.")

(setq org2jekyll-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c . n") 'org2jekyll-create-draft)
        (define-key map (kbd "C-c . p") 'org2jekyll-publish)
        (define-key map (kbd "C-c . P") 'org2jekyll-publish-posts)
        (define-key map (kbd "C-c . l") 'org2jekyll-list-posts)
        (define-key map (kbd "C-c . d") 'org2jekyll-list-drafts)
        map))

;;;###autoload
(defun org2jekyll-publish-posts ()
  "Publish all posts."
  (interactive)
  (->> (assoc org2jekyll-jekyll-layout-post org-publish-project-alist)
       org-publish-get-base-files
       (--filter (-> it org2jekyll-article-p org2jekyll-post-p))
       (mapc #'org2jekyll-publish-post)))

;;;###autoload
(defun org2jekyll-publish-pages ()
  "Publish all pages."
  (interactive)
  (->> (assoc org2jekyll-jekyll-layout-page org-publish-project-alist)
       org-publish-get-base-files
       (--filter (-> it org2jekyll-article-p org2jekyll-page-p))
       (mapc #'org2jekyll-publish-page)))

(defun org2jekyll-version ()
  "Little version helper"
  (interactive)
  (let ((version org2jekyll--version))
    (message "org2jekyll-version: %s" version)
    version))

(defun org2jekyll--bug-report ()
  "Compute the bug report for the user to include."
  (->> `("Please:"
         "- Describe your problem with clarity and conciceness (cf. https://www.gnu.org/software/emacs/manual/html_node/emacs/Understanding-Bug-Reporting.html)"
         "- Explicit your installation choice (melpa, marmalade, el-get, tarball, git clone...)."
         "- A sample of your configuration."
         "- Report the following message trace inside your issue."
         ""
         "System information:"
         ,(format "- system-type: %s" system-type)
         ,(format "- locale-coding-system: %s" locale-coding-system)
         ,(format "- emacs-version: %s" (emacs-version))
         ,(format "- org version: %s" (org-version))
         ,(format "- org2jekyll version: %s" org2jekyll--version)
         ,(format "- org2jekyll path: %s" (find-library-name "org2jekyll")))
       (s-join "\n")))

(defun org2jekyll-bug-report (&optional open-url)
  "Display a bug report message.
When OPEN-URL is filled, with universal argument (`C-u') is used,
opens new issue in org-trello's github tracker."
  (interactive "P")
  (when open-url
    (browse-url "https://github.com/ardumont/org2jekyll/issues/new"))
  (message (org2jekyll--bug-report)))


;;;###autoload
(define-minor-mode org2jekyll-mode
  "Functionality for publishing the current org-mode post to jekyll.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:
\\{org2jekyll-mode-map}"

  :init-value nil
  :lighter " o2j"
  :group 'org2jekyll
  :keymap org2jekyll-mode-map)

;; install/uninstall hook when activating/deactivating org2jekyll-mode
;; org2jekyll inserts the yaml when the publishing step is done, so now org
;; does all the publishing in concordance to what the user expects and we do
;; our update after it so Jekyll is happy.

(defvar org2jekyll-mode-on-hook nil "org2jekyll starting hook")
(setq org2jekyll-mode-off-hook nil) ;; for dev
;; install org2jekyll hook in org-publish when activating org2jekyll-mode
(add-hook 'org2jekyll-mode-on-hook
          (lambda ()
            (add-hook 'org-publish-after-publishing-hook 'org2jekyll-install-yaml-headers)))

(defvar org2jekyll-mode-off-hook '() "org2jekyll stoping hook")
(setq org2jekyll-mode-off-hook nil) ;; for dev
;; uninstall hook on org-publish
(add-hook 'org2jekyll-mode-off-hook
          (lambda () (remove-hook 'org-publish-after-publishing-hook 'org2jekyll-install-yaml-headers)))

;; Extend org-mode hyperlinks policy with a "local" link so we can publish
;; internal links which are then browsable when published
;; https://orgmode.org/manual/Adding-Hyperlink-Types.html#Adding-Hyperlink-Types

;; register new local links so org-mode exports them as is
(org-link-set-parameters "local" :export #'org2jekyll-local-link-export)

(defun org2jekyll-local-link-export (link description format)
  "Export a man page link from Org files."
  (let ((desc (or description link)))
    (if (string= format "html")
        (format "<a href=\"%s\">%s</a>" link desc)
      (org2jekyll-message "Unknown format %s, only dealing with html" format))))

(provide 'org2jekyll)
;;; org2jekyll.el ends here
