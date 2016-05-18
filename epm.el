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

(let ((epm-init-file "~/.epm.el"))
  (if (file-exists-p epm-init-file)
      ;; Using this instead of simply `load-file' is because I don't know how to
      ;; silence load message, something like:
      ;; Loading /Users/xcy/.epm.el (source)...
      (with-temp-buffer
        (insert-file-contents epm-init-file)
        (eval-buffer))
    (require 'package)
    (package-initialize)))

(defun epm-list ()
  (dolist (name
           (sort (mapcar #'symbol-name (mapcar #'car package-alist)) #'string<))
    (princ (format "%s\n" name))))

(defun epm-install (pkg-name)
  (package-install (intern pkg-name)))

(defun epm-delete (pkg-name)
  (package-delete (cadr (assoc (intern pkg-name) package-alist))))

(defun epm-info (pkg-name)
  ;; For feeding `package--builtins'
  (require 'finder-inf nil t)
  (let* (
         ;; Silence messages
         ;; https://github.com/cask/shut-up/pull/9#issuecomment-95485157
         (inhibit-message t)
         (pkg (intern pkg-name))
         (desc (or
                (cadr (assq pkg package-alist))
                (let ((built-in (assq pkg package--builtins)))
                  (if built-in
                      (package--from-builtin built-in)
                    (cadr (assq pkg package-archive-contents))))))
         (pkg-dir (if desc (package-desc-dir desc)
                    (error "Error: Unknown package: %s" pkg-name)))
         (reqs (if desc (package-desc-reqs desc)))
         (version (if desc (package-desc-version desc)))
         (archive (if desc (package-desc-archive desc)))
         (extras (and desc (package-desc-extras desc)))
         (homepage (cdr (assoc :url extras)))
         (keywords (if desc (package-desc--keywords desc)))
         (built-in (eq pkg-dir 'builtin))
         (installable (and archive (not built-in)))
         (status (if desc (package-desc-status desc) "orphan"))
         (signed (if desc (package-desc-signed desc))))
    (princ pkg-name)
    (princ " is ")
    (princ (if (memq (aref status 0) '(?a ?e ?i ?o ?u)) "an " "a "))
    (princ status)
    (princ " package.\n\n")
    ;; Status
    (cond (built-in
           (princ "  Status: Built-In"))
          (pkg-dir
           (princ "  Status: Installed"))
          (installable
           (princ "  Status: Not installed"))
          (t
           (princ (format "  Status: %s" (capitalize status)))))
    (princ "\n")
    ;; Archive
    (unless (and pkg-dir (not archive)) ; Installed pkgs don't have archive.
      (princ (format " Archive: %s" (or archive "n/a")))
      (princ "\n"))
    ;; Version
    (when version
      (princ (format " Version: %s\n" (package-version-join version))))
    (when desc
      (princ (format " Summary: %s\n" (package-desc-summary desc))))
    (when homepage
      (princ (format "Homepage: %s\n" homepage)))
    (princ "\n")))

(defun epm-search (query)
  (let ((packages (mapcar 'car package-archive-contents)))
    (setq packages (mapcar 'symbol-name packages))
    (dolist (match (cl-remove-duplicates
                    (delq nil
                          (mapcar (lambda (name)
                                    (if (string-match query name) name nil))
                                  packages))
                    :test #'equal))
      (princ (format "%s\n" match)))))

(defun epm--package-outdated-p (package)
  (let* ((pkg (cadr (assq package package-alist)))
         (old-version (and pkg (package-desc-version pkg)))
         (avaiable (cadr (assq package package-archive-contents)))
         (new-version (and avaiable (package-desc-version avaiable))))
    (and pkg avaiable
         (version-list-< old-version new-version)
         (list old-version new-version))))

(defun epm--outdated-packages ()
  (let (outdated)
    (dolist (package (mapcar #'car package-alist))
      (when (epm--package-outdated-p package)
        (push package outdated)))
    (nreverse outdated)))

(defun epm-outdated ()
  (dolist (package (epm--outdated-packages))
    (princ (format "%s\n" package))))

(defun epm-upgrade (pkg-name)
  (let ((upgrades (epm--outdated-packages)))
    (when (not (equal "" pkg-name))
      (if (epm--package-outdated-p (intern pkg-name))
          (setq upgrades (list (intern pkg-name)))
        (princ (format "Error: Unable to upgrade package: %s\n" pkg-name))
        (kill-emacs 1)))
    (dolist (package upgrades)
      (package-delete (cadr (assq package package-alist)))
      (package-install package))
    (princ (format "\nUpgrade %d package(s)\n" (length upgrades)))))

(provide 'epm)
;;; epm.el ends here
