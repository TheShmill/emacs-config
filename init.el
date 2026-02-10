(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(load-theme 'modus-vivendi-tinted)

(setq mac-command-modifier 'meta
      mac-option-modifier nil)

(require 'use-package)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(use-package magit
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package emacs
  :config
  (keymap-global-set "M-`" 'other-frame)
  (midnight-mode)
  (setq clean-buffer-list-delay-general 1)
  :custom
  (menu-bar-mode t) ;; I actually like having this on
  (scroll-bar-mode nil)
  (tool-bar-mode nil)
  (electric-pair-mode t) ; match parens
  (global-auto-revert-mode t)
  (tab-width 4)
  (make-backup-files nil)
  (auto-save-default nil)
  (indent-tabs-mode nil)
  ;; (tab-always-indent 'complete)
  (dired-dwim-target t)
  ;; Necessary because mac sed is goofy
  (Man-sed-command "gsed")
  ;; hide commands that don't support the current mode
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package tramp
  :ensure t
  :config
  ;; tramp optimizations
  (setq remote-file-name-inhibit-locks t)
  (setq tramp-use-scp-direct-remote-copying t)
  (setq remote-file-name-inhibit-auto-save t))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package vterm
  :ensure t)

(use-package pdf-tools
  :ensure t
  :init (pdf-tools-install))

(use-package pdf-view-restore
  :ensure t
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
  :custom
  (pdf-view-restore-filename (concat user-emacs-directory ".pdf-view-restore")))

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package marginalia ; provide descriptions for vertico stuff
  :ensure t
  :init (marginalia-mode))

(use-package paredit
  :ensure t
  :hook ((lisp-mode clojure-mode emacs-lisp-mode racket-mode) . enable-paredit-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package haskell-mode
  :ensure t)

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t
  :hook ((clojure-mode clojure-ts-mode) . cider-mode)
  :config
  (setq clojure-toplevel-inside-comment-form t))

(use-package sly
  :ensure t
  :custom
  (sly-compile-file-options '(:fasl-directory "/tmp/sly-fasls/"))
  (inferior-lisp-program "/opt/homebrew/bin/sbcl"))

(use-package avy
  :ensure t
  :config
  (avy-setup-default)
  (global-set-key (kbd "C-z") 'avy-goto-char-2))

(use-package corfu
  :ensure t
  :init (global-corfu-mode))

(use-package rustic
  :ensure t
  :after eglot
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (add-to-list 'eglot-server-programs
               '(gleam-ts-mode . ("gleam" "lsp")))
  (setq eglot-extend-to-xref t)
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-cargo-use-last-stored-arguments t))

(use-package rainbow-delimiters
  :ensure t
  :hook ((lisp-mode clojure-mode emacs-lisp-mode racket-mode racket-repl-mode) . rainbow-delimiters-mode))

(use-package elfeed
  :ensure t
  :custom
  (elfeed-feeds '("https://fasterthanli.me/index.xml"
                  "https://lexi-lambda.github.io/feeds/all.rss.xml"
                  "https://planet.haskell.org/atom.xml"
                  "https://tymoon.eu/api/reader/atom"
                  "https://tailrecursion.com/~alan/updates.xml"
                  "https://mmapped.blog/feed.xml"
                  "https://notgull.net/feed.xml"
                  "https://planet.lisp.org/rss20.xml"
                  "https://www.tedinski.com/feed.xml"
                  "https://matklad.github.io/feed.xml"
                  "https://clojure.org/feed.xml"
                  "https://without.boats/index.xml"
                  "https://blog.rust-lang.org/feed.xml"
                  "https://go.dev/blog/feed.atom"
                  "https://nora.codes/index.xml"
                  "https://what-if.xkcd.com/feed.atom"
                  "https://xkcd.com/atom.xml"
                  "https://planet.clojure.in/atom.xml"
                  "https://radekmie.dev/atom.xml"
                  "https://kerkour.com/feed.xml"
                  "https://erikarow.land/combined.xml"
                  "https://this-week-in-rust.org/rss.xml"
                  "https://tmandry.gitlab.io/blog/index.xml"
                  "https://blog.codinghorror.com/rss"
                  "https://pluralistic.net/feed/"
                  "https://eev.ee/feeds/atom.xml"
                  "https://cprss.s3.amazonaws.com/golangweekly.com.xml"
                  "https://www.wheresyoured.at/rss")))

(use-package htmlize
  :ensure t
  :custom
  (htmlize-output-type 'inline-css))

(use-package racket-mode
  :ensure t
  :config
  (require 'racket-xp)
  (add-hook 'racket-mode-hook #'racket-xp-mode)
  (add-hook 'racket-repl-mode-hook
            (lambda () (keymap-set racket-repl-mode-map "C-c M-o" 'racket-repl-clear-leaving-last-prompt))))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode 1))

(use-package gleam-ts-mode
  :ensure t
  :mode (rx ".gleam" eos)
  :config
  (add-to-list 'eglot-server-programs
               '(gleam-ts-mode . ("gleam" "lsp")))
  (add-hook 'gleam-ts-mode
            (lambda () (add-hook 'before-save-hook 'gleam-ts-format))))


(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-max-image-size '(256 . 256)))


(put 'list-timers 'disabled nil)
