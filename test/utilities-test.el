;;; utilities-test.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Antoine R. Dumont

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

(require 'utilities)
(require 'ert)

(ert-deftest test-orgtrello-tests-with-temp-buffer-and-return-buffer-content ()
  (should (string= "1
2
3
"
                   (org2jekyll-tests-with-temp-buffer-and-return-content
                    "line 1
line 2
line 3
"
                    (replace-regexp "line " "" nil (point-min) (point-max))))))


(provide 'utilities-test)
;;; utilities-test.el ends here
