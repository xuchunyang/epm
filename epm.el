;;; epm.el --- Emacs Package Manager                 -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang.me@gmail.com>
;; URL: https://github.com/xuchunyang/epm
;; Package-Requires: ((emacs "24.3") (epl "0.8"))
;; Version: 0.1

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

;; epm is a command-line wrapper for `package.el' that let you manage
;; Emacs packages from command-line
;;
;; After installing epm from Melpa or Melpa-stable, alias
;; `/path/to/epm/epm` as `epm`
;;
;;   $ alias epm='~/.emacs.d/elpa/epm-*/epm'
;;
;; Assuming the value of your `package-user-dir' is "~/.emacs.d/elpa"
;;
;; To use, invoke `epm' from shell
;;
;;   $ epm --help
;;
;; See the accompanying README.md for details.

;;; Code:

(require 'cl-lib)

(when (equal "yes" (getenv "EPM_RUNNING_P"))
  (let ((epm-init-file (expand-file-name (or (getenv "EPM_INIT_FILE") "~/.epm.el"))))
    (if (file-exists-p epm-init-file)
        (load epm-init-file nil 'no-message)
      (require 'package)
      (package-initialize))))

;; NOTE: This variable was added into Emacs at April 22 2016.
(when (boundp 'inhibit-message)
  (require 'cus-edit)
  (defun epm--inhibit-message (orig-func &rest r)
    "Let-bind `inhibit-message' around ORIG-FUNC."
    (let ((inhibit-message t))
      (apply orig-func r)))
  (advice-add 'customize-save-variable :around #'epm--inhibit-message))

(require 'epl)

(defun epm-ls-find-column-lengths (files window-width)
  ;; Borrowed from `eshell-ls-find-column-lengths'
  ;; FIXME: Tweak this function to drop some Eshell assumptions
  "Find the best fitting column lengths for FILES.
It will be returned as a vector, whose length is the number of columns
to use, and each member of which is the width of that column
\(including spacing)."
  (let* ((numcols 1)
         (width 0)
         (widths
          (mapcar
           (function
            (lambda (file)
              (+ 2 (length (car file)))))
           files))
         (max-width (+ window-width 2))
         col-widths
         colw)

    ;; refine it based on the following rows
    (while numcols
      (let* ((rows (ceiling (/ (length widths)
                               (float numcols))))
             (w widths)
             (len (* rows numcols))
             (index 0)
             (i 0))
        (setq width 0)
        (unless (or (= rows 0)
                    (<= (/ (length widths) (float rows))
                        (float (1- numcols))))
          (setq colw (make-vector numcols 0))
          (while (> len 0)
            (if (= i numcols)
                (setq i 0 index (1+ index)))
            (aset colw i
                  (max (aref colw i)
                       (or (nth (+ (* i rows) index) w) 0)))
            (setq len (1- len) i (1+ i)))
          (setq i 0)
          (while (< i numcols)
            (setq width (+ width (aref colw i))
                  i (1+ i))))
        (if (>= width max-width)
            (setq numcols nil)
          (if colw
              (setq col-widths colw))
          (if (>= numcols (length widths))
              (setq numcols nil)
            (setq numcols (1+ numcols))))))

    (if (not col-widths)
        (cons (vector max-width) files)
      (setq numcols (length col-widths))
      (let* ((rows (ceiling (/ (length widths)
                               (float numcols))))
             (len (* rows numcols))
             (newfiles (make-list len nil))
             (index 0)
             (i 0)
             (j 0))
        (while (< j len)
          (if (= i numcols)
              (setq i 0 index (1+ index)))
          (setcar (nthcdr j newfiles)
                  (nth (+ (* i rows) index) files))
          (setq j (1+ j) i (1+ i)))
        (cons col-widths newfiles)))))

(defun epm-ls-multi-column (strings)
  "Print STRINGS with multi-column like ls -C"
  (let* ((new-strings (mapcar #'list strings))
         (terminal-width
          (string-to-number (getenv "EPM_COLUMNS")))
         (col-vals (epm-ls-find-column-lengths
                    new-strings terminal-width))
         (col-widths (car col-vals))
         (entries (cdr col-vals)))
    (setq entries (mapcar #'car entries))
    (let ((len (length col-widths))
          (first t)
          (index 0))
      (dolist (elt entries)
        (when (and (not first) (zerop (% index len)))
          (princ "\n"))
        (when first
          (setq first nil))
        (when elt
          (princ (format (format "%%-%ds" (aref col-widths (% index len))) elt)))
        (setq index (+ 1 index)))
      (princ "\n"))))

(defun epm-ls-per-line (strings)
  "Print STRINGS one entry per line like ls -1"
  (dolist (s strings)
    (princ (format "%s\n" s))))

(defun epm-ls (strings)
  (if (equal "yes" (getenv "EPM_OUTPUT_TERMINAL_P"))
      (epm-ls-multi-column strings)
    (epm-ls-per-line strings)))

(defun epm-list ()
  (let ((pkg-names
         (nreverse
          (mapcar (lambda (p)
                    (symbol-name (epl-package-name p)))
                  (epl-installed-packages)))))
    (epm-ls pkg-names)))

(defun epm-install (pkg-name)
  (let ((package (car (epl-find-available-packages (intern pkg-name)))))
    (epl-package-install package)))

(defun epm-reinstall (pkg-name)
  (let ((package (epl-find-installed-package (intern pkg-name))))
    (if package
        (if (fboundp 'package-reinstall)
            (package-reinstall (epl-package-name package))
          (epl-package-delete package)
          (epl-package-install package))
      (princ (format "Error: Unable to reinstall \"%s\", since it is not installed\n" pkg-name))
      (kill-emacs 1))))

(defun epm-delete (pkg-name)
  ;; TODO Fix byte compiling warnnings
  (let ((package (epl-find-installed-package (intern pkg-name))))
    (if package
        (epl-package-delete package)
      (princ (format "Error: No such package: \"%s\"\n" pkg-name))
      (kill-emacs 1))))

(defun epm-info (pkg-name)
  (let ((p (or (epl-find-installed-package (intern pkg-name))
               (car (epl-find-available-packages (intern pkg-name))))))
    (unless p
      (princ (format "Error: No available package with the name \"%s\"\n" pkg-name))
      (kill-emacs 1))
    (let* ((installed-p (epl-package-installed-p p))
           (version (epl-package-version-string p))
           ;; Package description used in `package.el'
           (-p (epl-package-description p))
           (summary (and (fboundp 'package-desc-summary)
                         (package-desc-summary -p)))
           (archive (and (not installed-p)
                         (fboundp 'package-desc-archive)
                         (package-desc-archive -p)))
           (extras (and (fboundp 'package-desc-extras)
                        (package-desc-extras -p)))
           (homepage (cdr (assoc :url extras)))
           (reqs (epl-package-requirements p))
           (reqs-string (mapconcat
                         (lambda (r)
                           (concat (symbol-name (epl-requirement-name r))
                                   "-"
                                   (epl-requirement-version-string r)))
                         reqs ", ")))
      ;; Status
      (princ (format "  Status: %s\n" (if installed-p "Installed" "Not installed")))
      ;; Archive
      (unless installed-p
        (princ (format " Archive: %s\n" (or archive "n/a"))))
      ;; Version
      (princ (format " Version: %s\n" version))
      ;; Summary
      (when summary
        (princ (format " Summary: %s\n" summary)))
      ;; Homepage
      (when homepage
        (princ (format "Homepage: %s\n" homepage)))
      ;; Dependencies
      (when reqs
        (princ (format "Requires: %s\n" reqs-string))))))

(defun epm-search (query)
  (let ((pkg-names
         (mapcar (lambda (p)
                   (symbol-name (epl-package-name p)))
                 (epl-available-packages))))
    (epm-ls
     (cl-remove-duplicates
      (delq nil
            (mapcar (lambda (name)
                      (if (string-match query name) name nil))
                    pkg-names))
      :test #'equal))))

(defun epm-outdated ()
  (let ((pkg-names
         (mapcar (lambda (p)
                   (symbol-name (epl-package-name p)))
                 (epl-outdated-packages))))
    (epm-ls pkg-names)))

;; TODO Upgrade outdated dependencies as well
(defun epm-upgrade (pkg-name)
  (if (equal "" pkg-name)
      (epl-upgrade)
    (let ((pkg (intern pkg-name)))
      (unless (epl-package-installed-p pkg)
        (princ (format "Error: Not installed package: %s\n" pkg))
        (kill-emacs 1))
      (if (epl-package-outdated-p pkg)
          (epl-upgrade (list (epl-find-installed-package pkg)))
        (princ (format "%s is up to date\n" pkg))
        (kill-emacs 0)))))

(defconst epm-load-file load-file-name)

(defun epm-version ()
  (let (epm-version)
    (if (epl-package-installed-p 'epm)
        (setq epm-version
              (epl-package-version-string
               (epl-find-installed-package 'epm)))
      (let* ((gitdir (expand-file-name
                      ".git" (file-name-directory epm-load-file)))
             (default-directory gitdir))
        (when (file-exists-p gitdir)
          (with-temp-buffer
            (when (zerop (call-process-shell-command "git describe --tags" nil t))
              (goto-char 1)
              (setq epm-version (buffer-substring 1 (line-end-position))))))))
    (princ (format "EPM %s\nEmacs %s\n" (or epm-version "0.1") emacs-version))))

(defun epm-refresh (&optional show-new)
  (let (old-archives)
    (package-read-all-archive-contents)
    (setq old-archives package-archive-contents)
    (package-refresh-contents)
    (if show-new
	(let (new-packages)
	  (dolist (elt package-archive-contents)
	    (unless (assq (car elt) old-archives)
	      (push (car elt) new-packages)))
          (when new-packages
            ;; Because `epm-ls' needs a list of strings
            (princ (format "Added %d new package%s:\n"
                           (length new-packages)
                           (if (= (length new-packages) 1) "" "s")))
            (setq new-packages (mapcar #'symbol-name new-packages))
            (epm-ls new-packages))))))

(provide 'epm)
;;; epm.el ends here
