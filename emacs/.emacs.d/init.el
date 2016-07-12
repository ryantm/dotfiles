(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Packages
(package-initialize)
(add-to-list 'package-archives
             '("melpa-stable" . "https://melpa.org/packages/") t)

;; use-package organizes package configuration
(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package))

;; Shortcut bindings
(require 'bind-key)
;; Make minor modes less obvious in mode line
(require 'diminish nil t)
;; Use path to uniquely name buffers with the same name
(require 'uniquify)

(use-package spu
  :ensure t
  :defer 5 ;; defer package loading for 5 second
  :config (spu-package-upgrade-daily))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package autorevert
  :defer 5
  :config
  (setq auto-revert-verbose nil)
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode))

(use-package magit
  :ensure t
  :bind ("<f10>" . magit-status)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package fill-column-indicator
  :ensure t
  :defer 5
  :config
  (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
  (global-fci-mode 1))

(use-package whitespace
  :defer 5
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :config
  (global-whitespace-mode)
  (setq whitespace-global-modes '(not dired-mode magit-mode))
  (setq whitespace-style (delete 'lines whitespace-style)))

(use-package cus-edit+
  :defer t
  :ensure t)

(use-package color-theme-sanityinc-solarized
  :defer t
  :ensure t)

(use-package multiple-cursors
  :defer t
  :ensure t)

(use-package flycheck
  :defer t
  :ensure t)

(use-package haskell-mode
  :mode "\\.l?hs\\'"
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

  (use-package flycheck-haskell
    :defer t
    :ensure t)
  (use-package hi2
    :ensure t))

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :ensure t)

(use-package ledger-mode
  :mode "\\.ledger\\'"
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))

(use-package markdown-preview-mode
  :ensure t)

(use-package paren
  :defer 5
  :config
  (show-paren-mode))

(use-package lisp-mode
  :defer t
  :preface
  (defun my-lisp-mode-hook ()
    (unless lisp-mode-initialized
      (setq lisp-mode-initialized t)
      (use-package paredit
        :ensure t
        :diminish (paredit-mode))
      (use-package rainbow-delimiters
        :ensure t
        :diminish (rainbow-delimiters-mode))
      (use-package elisp-slime-nav
        :ensure t
        :diminish (elisp-slime-nav-mode))
      (paredit-mode)
      (rainbow-delimiters)
      (elisp-slime-nav)))
  (add-hook 'lisp-mode-hook 'my-lisp-mode-hook))

(use-package haml-mode
  :ensure t
  :mode "\\.haml\\'"
  :config
  (use-package flyspell)
  (use-package writeroom-mode
    :ensure t)
  (add-hook 'haml-mode-hook 'flyspell-mode)
  (add-hook 'haml-mode-hook 'auto-fill-mode)
  (add-hook 'haml-mode-hook 'writeroom-mode))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package ido
  :demand t
  :bind (("C-x b" . ido-switch-buffer))
  :config
  (ido-mode))

(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :ensure t
  :config (progn
            (setq ruby-deep-indent-paren-style nil)
            (use-package inf-ruby :ensure t))
  :init (defun ruby-send-whole-buffer ()
          (interactive)
          (save-buffer)
          (ruby-load-file (buffer-file-name (current-buffer)))))
  ;; :bind (("C-M-l" . ruby-forward-sexp)
  ;;        ("C-M-j" . ruby-backward-sexp)
  ;;        ("C-x e" . ruby-send-whole-buffer)
  ;;        ("C-x C-e" . ruby-send-whole-buffer)))

(use-package zeal-at-point
  :ensure t
  :bind ("C-c d" . zeal-at-point))

;; Customizations
(defconst custom-file-start-time (current-time))

(setq custom-file (expand-file-name "custom-file" user-emacs-directory))
(load custom-file)

(when (display-graphic-p)
  (let ((elapsed (float-time (time-subtract (current-time)
                                            custom-file-start-time))))
    (message "Loading custom-file...done (%.3fs)" elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract
                                           (current-time)
                                           custom-file-start-time))))
                 (message "Loading custom-file...done (%.3fs) [after-init]"
                          elapsed)))
            t))


;; Write backup and autosave files to their own directories
(setq backup-directory-alist
      `((".*" . ,(expand-file-name
                  (concat user-emacs-directory "backups")))))

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name
                (concat user-emacs-directory "autosaves")) t)))

(setq auto-save-list-file-prefix
      (expand-file-name
       (concat user-emacs-directory "autosaves")))

(setenv "PAGER" "")

;; Rebindings

; Macro for browsing a large magit commit history
(fset 'magit-rtm-down
   [tab ?n tab ?\C-l ?\C-l])

(global-unset-key (kbd "C-x C-b")) ;; Annoying Key (because it gets in
                                   ;; the way of switching buffers)
(setq my-rebinds '(
                     ("C-x C-l" goto-line)
                     ("C-x l" goto-line)
                     ("C-x e" eval-last-sexp)
                     ("<C-tab>" next-buffer)
                     ("<C-S-iso-lefttab>" previous-buffer)
                     ("C-=" text-scale-increase)
                     ("<C-mouse-4>" text-scale-increase)
                     ("C--" text-scale-decrease)
                     ("<C-mouse-5>" text-scale-decrease)
                     ("C-z" undo)
                     ("M-i" ido-goto-symbol)
                     ("C-x C-r" rgrep)
                     ("<f9>" magit-rtm-down)))

(defun do-rebindings (rebindings)
  (dolist (element rebindings)
    (let  ((keyboard-string (nth 0 element))
           (function (nth 1 element)))
      (global-set-key (read-kbd-macro keyboard-string) function))))
(do-rebindings my-rebinds)

(setq enable-local-variables :safe)

;;; Tabs
(setq indent 2)
(setq js-indent-level 2)
(add-hook 'shell-mode-hook
          (lambda ()
            (setq tab-width 8)))

;;; Appearance
(global-font-lock-mode t)
(setq inhibit-splash-screen t)

;;; Post initialization

(when (display-graphic-p)
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

;;; init.el ends here
