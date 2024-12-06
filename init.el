(defun start/org-babel-tangle-file ()
  "Tangle config.org into init.el"
  (when (string-equal (file-name-directory (buffer-file-name))
		  (expand-file-name user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil))
  (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'start/org-babel-tangle-file)))

(load-theme 'wombat)

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
  ;; hide commands that don't support the current mode
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  )

(use-package vterm
  :ensure t)

(use-package pdf-tools
  :ensure t)

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
  :config
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode))
