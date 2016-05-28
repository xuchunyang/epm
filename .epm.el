;; epm sample startup file

;; This file is supposed to bootstrap `package.el', refer your own
;; init file to find out what settings to populate this file.

;; Emacs China ELPA mirror <http://elpa.emacs-china.org/> is faster in China
(setq package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")
                         ("melpa" . "http://elpa.zilongshanren.com/melpa/")
                         ("org"   . "http://elpa.zilongshanren.com/org/")))

;; This is mandatory
(package-initialize)
