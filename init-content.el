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

(setq scroll-conservatively 101)  ; don't jump
(setq scroll-step 1)

(setq mouse-yank-at-point t)
(setq select-enable-primary t) ; yank to middle mouse

(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(3 ((shift) . hscroll)))

(global-unset-key [vertical-scroll-bar mouse-1])
(global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)

(electric-pair-mode t)

;;; themes

(add-to-list 'custom-theme-load-path user-emacs-directory)

(let ((fn (expand-file-name "blackboard-theme.el" user-emacs-directory))
      (checksum '4c7a1f0559674bf6d5dd06ec52c8badc5ba6e091f954ea364a020ed702665aa1))
  (add-to-list 'custom-safe-themes (symbol-name checksum))
  (unless (file-exists-p fn)
    (securl 'sha256 checksum
            "https://raw.githubusercontent.com/don9z/blackboard-theme/7a0d79410feb728ff5cce75c140fadc19a3f9a6d/blackboard-theme.el"
            fn)))

(let ((fn (expand-file-name "lush-theme.el" user-emacs-directory))
      (checksum 'a643ad8cf1c443a432ed5c370b96dbd493e95dddc9d82ec83eaddcb0276d4162))
  (add-to-list 'custom-safe-themes (symbol-name checksum))
  (unless (file-exists-p fn)
    (securl 'sha256 checksum
           "https://raw.githubusercontent.com/andre-richter/emacs-lush-theme/645e1959143532df8f7ef90e1184e9556df18af7/lush-theme.el"
           fn)))

(load-theme 'whiteboard)
;; (load-theme 'lush)
;; (load-theme 'blackboard)

;;; packages

(ow-enable-use-package)

(ow-use-packages
 flycheck
 powershell
 rainbow-delimiters
 sly-repl-ansi-color ac-sly
 sparql-mode
 toml-mode
 yaml-mode
 (evil
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
 (org-plus-contrib
   :mode ("\\.org\\'" . org-mode)
   :custom
   (org-adapt-indentation nil)
   (org-edit-src-content-indentation 0)
   :init
   (require 'ob-core)
   (org-babel-do-load-languages
    'org-babel-load-languages
    '((dot . t)
      (lisp . t)
      (python . t)
      (shell . t)
      (sparql . t)
      (sql . t))))
 (sly
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
 (undo-tree
   :custom
   (undo-tree-enable-undo-in-region nil)
   :init
   (global-undo-tree-mode)))

(if (>= emacs-major-version 25)
    (ow-use-packages
     magit
     racket-mode)

  ;; support for Emacs 24

  (setq straight-install-dir user-emacs-directory
        straight-current-profile 'pinned
        straight-repository-branch "develop"
        straight-check-for-modifications nil)

  (reval 'sha256 'c8c02c5c9f007abb5a0105d21ab3977fd5a9cea64d13d4f8d246fb0a5890c7f1
         "https://raw.githubusercontent.com/raxod502/straight.el/3277e1c9648b41dd5bfb239c067b8374ed2ec2bb/straight.el")
  (reval 'sha256 'f8516cbe60122b1e091ea8e5f87982cc3012649a8301c9e498d8f176ad4f39a9
         "https://raw.githubusercontent.com/raxod502/straight.el/846956eacd10830dc4afde2028c3e1f6f2945d33/straight-x.el")

  (add-to-list 'straight-profiles '(pinned . "pinned.el"))

  (autoload #'straight-x-pull-all "straight-x")
  (autoload #'straight-x-freeze-versions "straight-x")
  (straight--load-build-cache)
  (mapcar #'straight-use-recipes
          '((melpa
             :type git :host github
             :repo "melpa/melpa"
             :build nil)
            (gnu-elpa-mirror
             :type git :host github
             :repo "emacs-straight/gnu-elpa-mirror"
             :build nil)))
  (add-to-list 'straight-x-pinned-packages
               '("ghub" . "ae37bef2eb3afb8232bb0a6f7306a8da2390abf4"))
  (add-to-list 'straight-x-pinned-packages ; 2.13.0
               '("magit" . "e03685e813330a750c1d2e525a8f8c74901fccfb"))
  (add-to-list 'straight-x-pinned-packages
               '("racket-mode" . "f7917d7cb38f7faa03eb2146e21f2a2d4a3c0350"))
  (straight-use-package 'ghub)
  (straight-use-package 'magit)
  (straight-use-package 'racket-mode)
  (unless (string= (straight-vc-get-commit 'git (straight--repos-dir "magit"))
                   (cdr (assoc "magit" straight-x-pinned-packages)))
    (straight-x-pull-all)
    ;;(straight-use-package 'magit)
    ;;(straight-use-package 'racket-mode)
    (straight-freeze-versions t) ; have to force due to the hackery of our setup
    (straight-x-freeze-pinned-versions)
    (straight--symlink-package (straight--convert-recipe 'magit))
    (straight--symlink-package (straight--convert-recipe 'ghub))
    (straight--symlink-package (straight--convert-recipe 'racket-mode)))
  (use-package magit-popup)

  (require 'magit)
  (require 'racket-mode))

;;; reval update

(defun init-content--reval-update ()
  "Get the immutable url for the current remote version of this file."
  (reval-get-imm-github "tgbugs" "orgstrap" "init-content.el"))

;;;; init-content.el ends here
