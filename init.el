(defun start/org-babel-tangle-file ()
  "Tangle config.org into init.el"
  (when (string-equal (file-name-directory (buffer-file-name))
		  (expand-file-name user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil))
  (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'start/org-babel-tangle-file)))

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
  (auto-save-default nil))
