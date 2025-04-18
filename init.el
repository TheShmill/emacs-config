(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

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
  (tab-always-indent 'complete)
  ;; hide commands that don't support the current mode
  (read-extended-command-predicate #'command-completion-default-include-p))

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
  :hook ((clojure-mode clojure-ts-mode) . cider-mode))

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
  :init
  (setq rust-mode-treesitter-derive t)
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-cargo-use-last-stored-arguments t))
(load-theme 'wombat)
;; (use-package catppuccin-theme
;;   :ensure t
;;   :custom
;;   (catppuccin-theme 'macchiato)
;;   :hook (after-init . (lambda () (load-theme 'catppuccin))))

(use-package rainbow-delimiters
  :ensure t
  :hook ((lisp-mode clojure-mode emacs-lisp-mode racket-mode racket-repl-mode) . rainbow-delimiters-mode))

(use-package elfeed
  :ensure t
  :custom
  (elfeed-feeds '("https://fasterthanli.me/index.xml"
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
                  "https://blog.codinghorror.com/rss")))

(use-package elcord
  :ensure t)

(use-package htmlize
  :ensure t
  :custom
  (htmlize-output-type 'inline-css))

(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'gemini-1.5-pro-latest
        gptel-backend (gptel-make-gemini "Gemini" :stream t :key gptel-api-key)))

(global-set-key (kbd "C-S-O") 'join-line)

(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook (lambda () (add-hook 'before-save-hook 'eglot-format-buffer))))

;; (use-package zone
;;   :config
;;   (zone-when-idle 300)
;;   (setf zone-programs [zone-pgm-drip zone-pgm-putz-with-case zone-pgm-rotate-RL-variable]))

(use-package racket-mode
  :ensure t
  :config
  (require 'racket-xp)
  (add-hook 'racket-mode-hook #'racket-xp-mode))
