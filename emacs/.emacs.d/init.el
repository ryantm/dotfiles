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
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;(package-refresh-contents)

(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package))

(require 'bind-key)
(require 'diminish nil t)

(use-package
  magit
  :ensure t
  :bind ("M-C M" . magit-status))

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode))

(use-package elisp-slime-nav
  :ensure t
  :diminish (elisp-slime-nav-mode))

(use-package paredit
  :defer t
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package cus-edit+
  :defer t
  :ensure t)

(use-package color-theme-sanityinc-solarized
  :demand t
  :ensure t)

(use-package multiple-cursors
  :defer t
  :ensure t)

(use-package diminish
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package haskell-mode
  :mode "\\.l?hs\\'"
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(use-package flycheck-haskell
  :defer t
  :ensure t)

(use-package hi2
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))

(use-package paren
  :demand t
  :config
  (show-paren-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package flyspell
 :config
 (add-hook 'text-mode-hook 'flyspell-mode))

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

;; Customizations
(setq custom-file (expand-file-name "custom-file" user-emacs-directory))
(load custom-file)

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

;; Use path to uniquely name buffers with the same name
(require 'uniquify)

;; Rebindings
(global-unset-key (kbd "C-x C-b")) ;;Annoying Key (because it gets in the way of switching buffers)
(defvar my-rebinds '(
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
                     ("C-M-m" magit-status)))

(defun do-rebindings (rebindings)
  (dolist (element rebindings)
    (let  ((keyboard-string (nth 0 element))
           (function (nth 1 element)))
      (global-set-key (read-kbd-macro keyboard-string) function))))

(setq enable-local-variables :safe)

;; Tabs
(setq indent 2)
(setq js-indent-level 2)
(add-hook 'shell-mode-hook
          (lambda ()
            (setq tab-width 8)))

;; Appearance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-font-lock-mode t)
(do-rebindings my-rebinds)
(setq inhibit-splash-screen t)

;; Require libraries for use in initializations
(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))

;; Fullscreen magit-status
(when-available
 'magit-svn
 (progn
   (defadvice magit-status (around magit-fullscreen activate)
     (window-configuration-to-register :magit-fullscreen)
     ad-do-it
     (delete-other-windows))
   (defun magit-quit-session ()
     "Restores the previous window configuration and kills the magit buffer"
     (interactive)
     (kill-buffer)
     (jump-to-register :magit-fullscreen))
   (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)))

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
