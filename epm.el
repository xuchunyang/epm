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
;; See the accompanying README.md for details.

;;; Code:

(require 'cl-lib)

(when (equal "yes" (getenv "EPM_RUNNING_P"))
  (let ((epm-init-file "~/.epm.el"))
    (if (file-exists-p epm-init-file)
        ;; Using this instead of simply `load-file' is because I don't know how to
        ;; silence load message, something like:
        ;; Loading /Users/xcy/.epm.el (source)...
        (with-temp-buffer
          (insert-file-contents epm-init-file)
          (eval-buffer))
      (require 'package)
      (package-initialize))))

(require 'epl)

(defun epm-list ()
  (dolist (name
           (nreverse
            (mapcar #'epl-package-name (epl-installed-packages))))
    (princ (format "%s\n" name))))

(defun epm-install (pkg-name)
  (let ((package (car (epl-find-available-packages (intern pkg-name)))))
    (epl-package-install package)))

(defun epm-delete (pkg-name)
  (let ((package (epl-find-installed-package (intern pkg-name))))
    (epl-package-delete package)))

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
  (let ((packages (mapcar #'epl-package-name (epl-available-packages))))
    (setq packages (mapcar 'symbol-name packages))
    (dolist (match (cl-remove-duplicates
                    (delq nil
                          (mapcar (lambda (name)
                                    (if (string-match query name) name nil))
                                  packages))
                    :test #'equal))
      (princ (format "%s\n" match)))))

(defun epm-outdated ()
  (dolist (name
           (mapcar #'epl-package-name (epl-outdated-packages)))
    (princ (format "%s\n" name))))

(defun epm-upgrade (pkg-name)
  (if (equal "" pkg-name)
      (epl-upgrade)
    (let ((pkg (intern pkg-name)))
      (unless (epl-package-installed-p pkg)
        (princ (format "Error: Not installed package: %s\n" pkg))
        (kill-emacs 1))
      (if (epl-package-outdated-p pkg)
          (epl-upgrade (list (epl-find-installed-package pkg)))
        (princ (format "%s is up to date." pkg))
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

(provide 'epm)
;;; epm.el ends here
