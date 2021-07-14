;;;; init-content.el --- entry point -*- lexical-binding: t -*-

;; Author: Tom Gillespie
;; Homepage: https://github.com/tgbugs/orgstrap
;; Version: 9999
;; Package-Requires: ((emacs "24.4"))
;; Is-Version-Of: https://raw.githubusercontent.com/tgbugs/orgstrap/master/init-content.el
;; Reval-Get-Immutable: init-content--reval-update

;;;; License and Commentary

;; License:
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; init-content.el expects to be loaded using reval into an
;; environment with ow- and reval- present

;; init-content.el is compatible with `reval-update'.

;;; Code:

;;; preamble

(setq gc-cons-threshold 80000000)

(set-language-environment "UTF-8")

;;; os specific fixes

(cond ((eq system-type 'darwin)
       (setq exec-path (cons "/usr/local/bin" exec-path))))

;;; usability

(setq inhibit-startup-screen t)

(setq history-length t)
(savehist-mode t)

(setq mouse-yank-at-point t)
(setq select-enable-primary t) ; yank to middle mouse

(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(3 ((shift) . hscroll)))

(global-unset-key [vertical-scroll-bar mouse-1])
(global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)

(electric-pair-mode t)

;;; theme

(load-theme 'whiteboard)

;;; packages

(ow-enable-use-package)

(ow-use-packages flycheck
                 magit
                 powershell
                 racket-mode
                 rainbow-delimiters
                 sly-repl-ansi-color ac-sly
                 sparql-mode
                 toml-mode
                 yaml-mode)

(use-package evil
  :custom
  (evil-want-Y-yank-to-eol t)
  (evil-symbol-word-search t)
  (evil-undo-system 'undo-tree)
  :bind (:map evil-normal-state-map
              ("M-.")
              ("Y" . "y$") ; still needed because `evil-want-Y-yank-to-eol' is broken in some cases
              ("<return>" . evil-ex-nohighlight))
  :init
  (require 'evil)
  (evil-mode 1)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search))

(use-package org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (lisp . t)
     (python . t)
     (shell . t)
     (sparql . t)
     (sql . t))))

(use-package sly
  :custom
  (sly-net-coding-system 'utf-8-unix)
  (inferior-lisp-program "sbcl")
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color)
  (setq sly-lisp-implementations
        '((sbcl ("sbcl")
                :coding-system utf-8-unix)
          (clisp ("clisp"))))
  :config
  (sly-setup))

(use-package undo-tree
  :custom
  (undo-tree-enable-undo-in-region nil)
  :init
  (global-undo-tree-mode))

;;; reval update

(defun init-content--reval-update ()
  "Get the immutable url for the current remote version of this file."
  (reval-get-imm-github "tgbugs" "orgstrap" "init-content.el"))

;;;; init-content.el ends here
