# epm

## Introduction

epm is a command-line wrapper for `package.el` that serves as an Emacs Package Manager on command-line.

## Requirements

- Emacs 24.3 or higher
- `epl` 0.8 or higher

## Installation

- Obtain epm
- Add `/path/to/epm` to your `PATH` or alias `/path/to/epm/epm` as `epm`

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
    install PACKAGE     Install a package
    list                List installed packages
    delete  PACKAGE     Delete a package
    info    PACKAGE     Display information about a package
    search  PATTERN     Search packages
    outdated            List outdated packages
    upgrade [PACKAGE]   Upgrade a outdated package (or all outdated packages)

Options:
    -h, --help          Display this message
    -v, --version       Print version info and exit

Environment Variable:
    EMACS               The command name or executable path of Emacs

Startup File:
    ~/.epm.el           The file contains code to bootstrap package.el
                        Refer to /Users/xcy/Projects/epm/.epm.el as an example
```

Before you can actually use epm to do some real package management,
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
