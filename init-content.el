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

;;; testing helper

;; don't compile packages since it is faster to run interpreted
;; than to wait for compile unless we pre-build an image
;(require 'package)
;(defun package--compile (p) (ignore p))

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

;;; packages

(ow-enable-use-package nil '(vterm zmq))

;; hack to remove built-in org package so that we can download the
;; version from gnu elpa
(assq-delete-all 'org package--builtins)
(assq-delete-all 'org package--builtin-versions)

;; check for site packages
(when ow-site-packages
  (ow-populate-site-packages))

;; `org-assert-version' is a nightmare when trying to use orgstrap and a newer version of org
(defmacro org-assert-version () 't) ; SILENCE (reminder, don't byte compile org)

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
 (sly
   :custom
   (sly-net-coding-system 'utf-8-unix)
   (inferior-lisp-program "sbcl")
   :bind (:map sly-mrepl-mode-map
               ("C-<up>" . sly-mrepl-previous-input-or-button)
               ("C-<down>" . sly-mrepl-next-input-or-button))
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
   (undo-tree-auto-save-history nil)
   (undo-tree-history-directory-alist
    (list (cons "." (expand-file-name "undo-tree-backup" user-emacs-directory))))
   :init
   (global-undo-tree-mode)))

(if (>= emacs-major-version 26)
  (ow-use-packages
   markdown-mode)

  ;; support for Emacs 25

  ;; TODO

  )

(if (>= emacs-major-version 25)
    (progn
      (let ((no-byte-compile t)) (ow-use-packages org))
      (ow-use-packages
       magit
       racket-mode))

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
  (add-to-list 'straight-x-pinned-packages ; 9.4.6
               '("org" . "652430128896e690dc6ef2a83891a1209094b3da"))
  (add-to-list 'straight-x-pinned-packages
               '("ghub" . "ae37bef2eb3afb8232bb0a6f7306a8da2390abf4"))
  (add-to-list 'straight-x-pinned-packages ; last before dropping 24.4 support
               ;; "cf6b7e1c713f1dfd18340253d218cb006720dd91" ; this is the last 25.1 supporting for when we need it above
               '("markdown-mode" . "365697302345f8b9dc10bc975477452a282f7ae0"))
  (add-to-list 'straight-x-pinned-packages ; 3.2.0ish right before dropping 24 support
               '("with-editor" . "8c7672c04bb606f01a41b558a922dee1127568cc"))
  (add-to-list 'straight-x-pinned-packages ; 2.13.0
               '("magit" . "e03685e813330a750c1d2e525a8f8c74901fccfb"))
  (add-to-list 'straight-x-pinned-packages
               '("racket-mode" . "f7917d7cb38f7faa03eb2146e21f2a2d4a3c0350"))
  ;;; clone
  (straight-use-package 'ghub nil t)
  (straight-use-package 'markdown-mode nil t)
  (straight-use-package 'racket-mode nil t)
  (straight-use-package 'with-editor nil t)
  (let ((org-recipe
         `(org :type git
               :host nil
               :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
               :local-repo "org"
               :files ("lisp/*.el*")
               :build ,(let ((make (if (eq system-type 'berkeley-unix)
                                       "gmake"
                                     "make"))
                             (emacs (concat "EMACS="
                                            invocation-directory
                                            invocation-name)))
                         `(,make "oldorg" ,emacs))))
        (magit-recipe
         '(magit :type git
                 :host github
                 :repo "magit/magit"
                 :flavor melpa
                 :files ("lisp/*.el*"))))
    (straight-use-package magit-recipe nil t)
    (let ((no-byte-compile t))
      (straight-use-package org-recipe nil t))
    ;;; checkout and freeze
    (unless (string= (straight-vc-get-commit 'git (straight--repos-dir "magit"))
                     (cdr (assoc "magit" straight-x-pinned-packages)))
      (straight-x-pull-all)
      (straight-freeze-versions t) ; have to force due to the hackery of our setup
      (straight-x-freeze-pinned-versions)
      (straight-x-thaw-pinned-versions) ; turns out thaw != unlock
      (cl-loop
       for recipe in (list 'with-editor magit-recipe 'ghub 'markdown-mode 'racket-mode org-recipe)
       do (let ((sr (straight--convert-recipe magit-recipe)))
            (straight--run-build-commands sr)
            (straight--symlink-package sr))))
    ;;; build and install
    (straight-use-package 'ghub)
    (straight-use-package 'with-editor)
    (straight-use-package magit-recipe)
    (straight-use-package 'markdown-mode)
    (straight-use-package 'racket-mode)
    (let ((no-byte-compile t))
      (straight-use-package org-recipe)))

  ;;(use-package with-editor) ; magit can only fetch after 2nd launch
  (use-package magit-popup) ; magit
  (use-package pos-tip) ; racket-mode

  ;;(require 'org)
  ; cl-defgeneric -> magit not at right commit
  ; pos-tip -> racket-mode not at right commit
  (require 'magit)
  (require 'markdown-mode)
  (require 'racket-mode))

(ow-use-packages
 (org
  :no-require t
  :mode ("\\.org\\'" . org-mode)
  :bind (:map org-mode-map
              ("<tab>" . org-cycle))
  :custom
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  :init
  ;; FIXME Emacs 24 during the first pass byte compile of ob-core.el
  ;; the bytecode file that is produced is mangled in such a way that
  ;; org-babel-check-evaluate always returns nil, fix is to delete
  ;; ob-core.elc and then recompile it
  (require 'ob-core)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (lisp . t)
     (python . t)
     (shell . t)
     (sparql . t)
     (sql . t)))))

;;; themes
;;;; must come after packages so that require org-faces doesn't load
;;;; the version of org that is bundled with Emacs

(load-theme 'whiteboard)
(ow--tweak-whiteboard)

;; (load-theme 'lush)
;; (load-theme 'blackboard)

;;; reval update

(defun init-content--reval-update ()
  "Get the immutable url for the current remote version of this file."
  (reval-get-imm-github "tgbugs" "orgstrap" "init-content.el"))

;;;; init-content.el ends here
