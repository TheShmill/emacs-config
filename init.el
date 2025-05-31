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
  :config
  (keymap-global-set "M-`" 'other-frame)
  (keymap-global-set "C-t" 'eshell)
  (keymap-global-set "M-t" 'vterm)
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
  :after eglot
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (add-to-list 'eglot-server-programs
               '(gleam-ts-mode . ("gleam" "lsp")))
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
                  "https://lexi-lambda.github.io/feeds/all.rss.xml"
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

;; (use-package elcord
;;   :ensure t
;;   :config
;;   ;; https://github.com/Mstrodl/elcord/issues/17
;;   (defun elcord--disable-when-everything-closed (f)
;;     ;; only pause if we are about to delete the last visible frame
;;     (when (let ((frames (delete f (visible-frame-list))))
;;             (or (null frames)
;;                 (and (null (cdr frames))
;;                      (eq (car frames) terminal-frame))))
;;       ;; stop updates and store elapsed time
;;       (setq elcord--startup-time (string-to-number (format-time-string "%s" (time-subtract nil elcord--startup-time))))
;;       (elcord--disable)
;;       ;; Stop reconnect timer, idk why elcord--disable doesn't
;;       (when elcord--reconnect-timer
;;         (cancel-timer elcord--reconnect-timer))

;;       ;; reenable when a new frame gets made
;;       (add-hook 'after-make-frame-functions 'elcord--enable-when-frame-created)))

;;   (defun elcord--enable-when-frame-created (f)
;;     (ignore f)
;;     ;; resume elapsed time and continue updates
;;     (setq elcord--startup-time (string-to-number (format-time-string "%s" (time-subtract nil elcord--startup-time))))

;;     ;; FIXME: sends an error message if no frames were open for longer than elcord-idle-time
;;     ;; Cause: the timer triggers instantly, but depends on elcord--update-presence-timer being a timer
;;     ;; This variable is nil until elcord receives a message from Discord.
;;     ;; Possible solution: Delay this until the next time emacs is not idle? Perhaps whenever the next input is entered.
;;     ;; This could be achieved with a hook on pre-command-hook and then removing the hook once it is called once
;;     (when elcord-idle-timer
;;       (run-with-idle-timer elcord-idle-timer t 'elcord--start-idle))
;;     (elcord--start-reconnect)

;;     ;; We only want to run this function on the first frame that gets created
;;     (remove-hook 'after-make-frame-functions 'elcord--enable-when-frame-created))


;;   (add-hook 'delete-frame-functions 'elcord--disable-when-everything-closed))

(use-package htmlize
  :ensure t
  :custom
  (htmlize-output-type 'inline-css))

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
  (add-hook 'racket-mode-hook #'racket-xp-mode)
  (add-hook 'racket-repl-mode-hook
            (lambda () (keymap-set racket-repl-mode-map "C-c M-o" 'racket-repl-clear-leaving-last-prompt))))

(put 'narrow-to-region 'disabled nil)

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
  :ensure t)

(use-package treemacs
  :ensure t)

(use-package evil
  :ensure t
  :config
  (evil-mode)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-global-set-key 'normal (kbd "<leader>fs") 'save-buffer)
  (evil-global-set-key 'normal (kbd "<leader>ff") 'find-file)
  (evil-global-set-key 'normal (kbd "<leader>pf") 'project-find-file)
  (evil-global-set-key 'normal (kbd "<leader>pp") 'project-switch-project)
  (evil-global-set-key 'normal (kbd "s") 'avy-goto-char-2)
  (evil-global-set-key 'normal (kbd "<leader>g") 'magit)
  :init
  )

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-indent-plus
  :ensure t
  :after evil
  :config
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
  (define-key evil-inner-text-objects-map "J" 'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "J" 'evil-indent-plus-a-indent-up-down))
