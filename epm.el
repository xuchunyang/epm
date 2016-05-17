;;; epm.el --- Emacs Package Manager                 -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang.me@gmail.com>

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

;;; Code:

(require 'package)
(package-initialize)

(defun epm-list ()
  (dolist (name
           (sort (mapcar #'symbol-name (mapcar #'car package-alist)) #'string<))
    (princ (format "%s\n" name))))

(defun epm-install (pkg-name)
  (package-install (intern pkg-name)))

(defun epm-delete (pkg-name)
  (package-delete (cadr (assoc (intern pkg-name) package-alist))))

(provide 'epm)
;;; epm.el ends here
