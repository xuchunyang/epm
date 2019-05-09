;; epm sample startup file

;; This file is supposed to bootstrap `package.el', refer your own
;; init file to find out what settings to populate this file.

(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; This is mandatory
(package-initialize)
