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
  :hook ((lisp-mode clojure-mode emacs-lisp-mode) . enable-paredit-mode))

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
  (inferior-lisp-program "/opt/homebrew/bin/sbcl"))

(use-package avy
  :ensure t
  :config
  (avy-setup-default)
  (global-set-key (kbd "C-r") 'avy-goto-char-2))

(use-package corfu
  :ensure t
  :init (global-corfu-mode))

(use-package rustic
  :ensure t
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-cargo-use-last-stored-arguments t))

(use-package catppuccin-theme
  :ensure t
  :hook (after-init . (lambda () (load-theme 'catppuccin))))

(use-package rainbow-delimiters
  :ensure t
  :hook ((lisp-mode clojure-mode emacs-lisp-mode) . rainbow-delimiters-mode))
