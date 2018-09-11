(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Packages
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/") ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(when (not (package-installed-p 'use-package))
    (package-refresh-contents)
    (package-install 'use-package))

(when (not (window-system))
  (send-string-to-terminal "\033]12;black\007"))

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

;; broken on nixos
;; (use-package bash-completion
;;   :ensure t
;;   :config
;;   (bash-completion-setup))

;; broken in emacs 25
;; (use-package spu
;;   :ensure t
;;   :defer 5 ;; defer package loading for 5 second
;;   :config (spu-package-upgrade-daily))

(use-package autorevert
  :defer 5
  :config
  (setq auto-revert-verbose nil)
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode))

; Macro for browsing a large magit commit history
(fset 'magit-rtm-down
   [tab ?n tab ?\C-l ?\C-l])

(fset 'magit-rtm-up
   [tab ?p tab ?\C-l ?\C-l])

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         :map magit-status-mode-map
         ("C-M-n" . magit-rtm-down)
         ("C-M-p" . magit-rtm-up))
  :init
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package whitespace
  :ensure t
  :defer 5
  :bind (("C-c w" . global-whitespace-mode))
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :config
  (global-whitespace-mode))


(use-package color-theme-sanityinc-solarized
  :defer t
  :ensure t)

(use-package multiple-cursors
  :defer t
  :ensure t)

(use-package flycheck
  :defer t
  :ensure t)

(use-package c-mode :mode "\\.ino\\'")


(use-package haskell-mode
  :mode "\\.l?hs\\'"
  :ensure t
  :bind ("C-c ," . haskell-mode-format-imports)
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

  (use-package flycheck-haskell
    :defer t
    :ensure t)
  (use-package hi2
    :ensure t))

(use-package purescript-mode
  :mode "\\.purs\\'"
  :ensure t)

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
  :ensure t
  :defer 5
  :config
  (show-paren-mode))

(use-package shell
  :ensure t
  :config
  (defun my-shell-mode-hook ()
    (setq tab-width 8)
    (dirtrack-mode))
  (add-hook 'shell-mode-hook 'my-shell-mode-hook))

  (use-package paredit
    :ensure t
    :diminish (paredit-mode))
  (use-package rainbow-delimiters
    :ensure t
    :diminish (rainbow-delimiters-mode))
  (use-package elisp-slime-nav
    :ensure t
    :diminish (elisp-slime-nav-mode))

(use-package emacs-lisp-mode
  :ensure t
  :defer t
  :preface
  (defun my-emacs-lisp-mode-hook ()
    (paredit-mode)
    (rainbow-delimiters-mode)
    (elisp-slime-nav-mode))
  (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook))

(use-package haml-mode
  :ensure t
  :mode "\\.haml\\'"
  :config
  (use-package flyspell)
  (add-hook 'haml-mode-hook 'flyspell-mode))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package helm
  :ensure t
  :demand t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-x f" . helm-recentf)
         ("M-y" . helm-show-kill-ring))
  :diminish (helm-mode)
  :config (progn
            (setq helm-buffers-fuzzy-matching t)
            (helm-mode 1)))

(use-package ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :interpreter "ruby"
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

(setq custom-file (expand-file-name "custom-file.el" user-emacs-directory))
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

(global-unset-key (kbd "C-x C-b")) ;; Annoying Key (because it gets in
                                   ;; the way of switching buffers)

; mousewheel and C-+ C-- scrolling
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

(setq my-rebinds '(
                     ("C-x C-l" goto-line)
                     ("C-x l" goto-line)
                     ("C-x e" eval-last-sexp)
                     ("<C-tab>" next-buffer)
                     ("<C-S-iso-lefttab>" previous-buffer)
                     ("M-i" ido-goto-symbol)
                     ("C-x C-r" rgrep)))

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
