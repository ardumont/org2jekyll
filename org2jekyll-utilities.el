;;; org2jekyll-utilities.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2017  Antoine R. Dumont

;; Author: Antoine R. Dumont <tony@dagobah>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defmacro org2jekyll-tests-with-temp-buffer (text body-test)
  "A `org-mode' mode buffer helper test on buffer.
TEXT is the content of the buffer.
BODY-TEST is the assertion to test on the buffer.
NB-LINES-FORWARD is the number of lines to get back to."
  `(with-temp-buffer
     (org-mode)
     (org2jekyll-mode)
     (insert ,text)
     ,body-test))

(defmacro org2jekyll-tests-with-temp-buffer-and-return-content (text body-test)
  "A `org-mode' mode buffer helper test on buffer.
TEXT is the content of the buffer.
BODY-TEST is the assertion to test on the buffer.
NB-LINES-FORWARD is the number of lines to get back to."
  `(with-temp-buffer
     (org-mode)
     (org2jekyll-mode)
     (insert ,text)
     ,body-test
     (buffer-substring-no-properties (point-min) (point-max))))

(provide 'org2jekyll-utilities)
;;; org2jekyll-utilities.el ends here
