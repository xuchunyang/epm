# Emacs Package Manager

[![MELPA](https://melpa.org/packages/epm-badge.svg)](https://melpa.org/#/epm)
[![MELPA Stable](https://stable.melpa.org/packages/epm-badge.svg)](https://stable.melpa.org/#/epm)

## Introduction

EPM is a command-line wrapper for `package.el` that serves as an Emacs Package Manager on command-line.

## Requirements

- Emacs 24.3 or higher
- `epl` 0.8 or higher

## Installation

### ELPA

EPM is available from Melpa and Melpa-stable, you can install EPM and its dependency easily

    M-x package-install RET epm RET

Since the directory package is installed to is not stable, you can setup an alias

    $ alias epm='~/.emacs.d/elpa/epm-*/epm'

### Manually

Install EPM's dependency epl

    M-x package-install RET epl RET

Obtain EPM via git

    $ git clone https://github.com/xuchunyang/epm.git

Add EPM's directory to your PATH

    $ export PATH=/path/to/epm/:$PATH

## Usage

Run `epm --help` to learn the usage, below is the ouput on my machine:


```
$ epm --help
epm - Emacs Package Manager

Usage:
    epm <command>
    epm [option]

Commands:
    refresh             Download package archives
    install   PACKAGE   Install a package
    reinstall PACKAGE   Reinstall a package
    list                List installed packages
    delete    PACKAGE   Delete a package
    info      PACKAGE   Display information about a package
    search    PATTERN   Search packages
    outdated            List outdated packages
    upgrade   [PACKAGE] Upgrade a outdated package (or all outdated packages)

Options:
    -h, --help          Display this message
    -v, --version       Print version info and exit

Environment Variable:
    EMACS               The command name or executable path of Emacs

Startup File:
    ~/.epm.el           The file contains code to bootstrap package.el
                        Refer to /Users/xcy/Projects/epm/.epm.el as an example
```

Before you can actually use EPM to do some real package management,
you may need to set `EMACS` and add `~/.epm.el`.

## Sample Startup File

```emacs-lisp
;; Emacs China ELPA mirror <http://elpa.emacs-china.org/> is faster in China
(setq package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")
                         ("melpa" . "http://elpa.zilongshanren.com/melpa/")
                         ("org"   . "http://elpa.zilongshanren.com/org/")))

;; This is mandatory
(package-initialize)
```

See also [`.epm.el`](./.epm.el)
