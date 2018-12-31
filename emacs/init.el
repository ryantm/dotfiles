(when (getenv "INSIDE_EMACS")
  (kill-emacs))

(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

(when (not (window-system))
  (send-string-to-terminal "\033]12;black\007"))

(eval-when-compile
  (require 'package)
  (package-initialize)
  (defvar use-package-verbose t)
  (require 'use-package))


(use-package bind-key)
(use-package diminish)
(use-package uniquify
  :defer 5)

(use-package mule
  :custom
  (set-terminal-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  :config
  (setq locale-coding-system 'utf-8))

(use-package bash-completion
  :disabled
  :config
  (bash-completion-setup))

(use-package autorevert
  :defer 5
  :config
  (setq auto-revert-verbose nil)
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode))

;; Macro for browsing a large magit commit history
(fset 'magit-rtm-down
      [tab ?n tab ?\C-l ?\C-l])

(fset 'magit-rtm-up
      [tab ?p tab ?\C-l ?\C-l])

(use-package magit
  :commands (magit-status)
  :bind (("C-x g" . magit-status)
         :map magit-status-mode-map
         ("C-M-n" . magit-rtm-down)
         ("C-M-p" . magit-rtm-up)))

(use-package whitespace
  :defer 5
  :bind (("C-c w" . global-whitespace-mode))
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :config
  (global-whitespace-mode))


(use-package color-theme-sanityinc-solarized)

(use-package multiple-cursors
  :defer t)

(use-package flycheck
  :defer t)

(use-package cc-mode
  :mode ("\\.ino\\'" . c-mode))

;; (use-package intero
;;   :custom
;;   (intero-global-mode 1))

(use-package haskell-mode
  :mode "\\.l?hs\\'"
  :bind ("C-c ," . haskell-mode-format-imports)
  :hook (before-save . hindent-reformat-buffer))

(use-package purescript-mode
  :mode "\\.purs\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package ledger-mode
  :mode "\\.ledger\\'")

(use-package markdown-preview-mode
  :hook (gfm-mode markdown-mode))

(use-package markdown-mode
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))

(use-package paren
  :hook (prog-mode . show-paren-mode))

(use-package dirtrack
  :hook (shell-mode . dirtrack-mode))

(use-package shell
  :commands shell
  :config
  (setq tab-width 8))

(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode)
  :diminish (paredit-mode))

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode)
  :diminish (rainbow-delimiters-mode))

(use-package elisp-slime-nav
  :hook (emacs-lisp-mode . elisp-slime-nav-mode)
  :diminish (elisp-slime-nav-mode))

(use-package elisp-mode
  :mode ("\\.el\\'" . emacs-lisp-mode))

(use-package flyspell
  :hook (haml-mode . flyspell-mode))

(use-package haml-mode
  :mode "\\.haml\\'")

(use-package nix-mode
  :mode "\\.nix\\'"
  :functions nix-indent-line
  :custom
  (nix-indent-function #'nix-indent-line))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-x f" . helm-recentf)
         ("M-y" . helm-show-kill-ring))
  :diminish (helm-mode)
  :custom
  (helm-buffers-fuzzy-matching t))

(use-package inf-ruby
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :custom
  (ruby-deep-indent-paren-style nil))

(use-package zeal-at-point
  :bind ("C-c d" . zeal-at-point))

;; Customizations
(defconst custom-file-start-time (current-time))

(setq custom-file (expand-file-name "custom-file.elc" user-emacs-directory))
(load custom-file)

(unless noninteractive
  (let ((elapsed (float-time (time-subtract (current-time)
                                            custom-file-start-time))))
    (message "Loading custom-file...done (%.3fs)" elapsed)))


;; Write backup and autosave files to their own directories
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.saves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 vc-make-backup-files t)

;; (setq backup-directory-alist
;;       `((".*" . ,(expand-file-name
;;                   (concat user-emacs-directory "backups")))))

;; (setq auto-save-file-name-transforms
;;       `((".*" ,(expand-file-name
;;                 (concat user-emacs-directory "autosaves")) t)))

;; (setq auto-save-list-file-prefix
;;       (expand-file-name
;;        (concat user-emacs-directory "autosaves")))

(setenv "PAGER" "")

;; Rebindings

;; Annoying Key (because it gets in the way of switching buffers)
(global-unset-key (kbd "C-x C-b"))

;; mousewheel and C-+ C-- scrolling
(defun scale-to (f)
  (set-face-attribute 'default nil :height
                      (round f)))

(defun scale-by (f)
  (scale-to (* f (face-attribute 'default :height))))

(defun scale-up () (interactive)
       (scale-by 1.1))

(defun scale-down () (interactive)
       (scale-by (/ 1 1.1)))

(defun scale-reset () (interactive)
       (scale-to 100))

(global-set-key (kbd "C-=") #'scale-up)
(global-set-key (kbd "C--") #'scale-down)
(global-set-key (kbd "C-0") #'scale-reset)
(global-set-key [C-mouse-4] #'scale-up)
(global-set-key [C-mouse-5] #'scale-down)
(global-set-key (kbd "M-m") 'mc/edit-lines)

(let ((rebindings '(("C-x C-l" goto-line)
                    ("C-x l" goto-line)
                    ("C-x e" eval-last-sexp)
                    ("<C-tab>" next-buffer)
                    ("<C-S-iso-lefttab>" previous-buffer)
                    ("M-i" ido-goto-symbol)
                    ("C-x C-r" rgrep))))
  (dolist (element rebindings)
    (let  ((keyboard-string (nth 0 element))
           (function (nth 1 element)))
      (global-set-key (read-kbd-macro keyboard-string) function))))

(setq enable-local-variables :safe)

;;; Tabs
(setq js-indent-level 2)
(setq tab-width 2)

;;; Appearance
(global-font-lock-mode t)
(setq inhibit-splash-screen t)

;;; Post initialization

(unless noninteractive
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
