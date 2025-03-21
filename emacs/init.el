(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

;; Customizations
(defconst custom-file-start-time (current-time))

(setq custom-file (expand-file-name "custom-file.el" user-emacs-directory))
(load custom-file)
(setq custom-file (expand-file-name "~/p/dotfiles/emacs/custom-file.el"))

(unless noninteractive
  (let ((elapsed (float-time (time-subtract (current-time)
                                            custom-file-start-time))))
    (message "Loading custom-file...done (%.3fs)" elapsed)))

(eval-when-compile
  (require 'package)
  (package-initialize)
  (defvar use-package-verbose t)
  (require 'use-package))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (go-mode . go-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))

(use-package bind-key)
(use-package diminish)
(use-package uniquify
  :defer 5)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package eglot
 :hook (prog-mode . eglot-ensure)
 :config
 (setq eglot-server-programs
       '((typescript-ts-mode . ("typescript-language-server" "--stdio"))
         (tsx-ts-mode . ("typescript-language-server" "--stdio"))
         (go-ts-mode . ("gopls"))
         (json-ts-mode . ("vscode-json-languageserver" "--stdio"))
         (js-json-mode . ("vscode-json-languageserver" "--stdio"))
         (python-ts-mode . ("basedpyright-langserver" "--stdio"))
         (c++-mode . ("clangd"))))
 (setq-default
       eglot-workspace-configuration
       '(:basedpyright.analysis (
           :diagnosticMode "openFilesOnly"))))

(use-package c++-mode
  :hook (c++-mode . eglot-ensure))

(setq font-lock-maximum-decoration t)
(setq-default show-trailing-whitespace nil)

(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

(use-package python-ts-mode
  :hook (python-ts-mode . eglot-ensure)
        (python-ts-mode . eglot-format-buffer-on-save))

(use-package go-ts-mode
  :hook ((go-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-format-buffer-on-save)))

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

(use-package magit
  :commands (magit-status)
  :bind (("C-x g" . magit-status)
         :map magit-status-mode-map
         ))

(use-package whitespace
  :defer 5
  :bind (("C-c w" . global-whitespace-mode))
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :config
  (global-whitespace-mode))

(use-package multiple-cursors
  :defer t)

(use-package flycheck
  :defer t)

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

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

(defun prettier ()
  (add-hook 'before-save-hook #'prettier-prettify -10 t))

(use-package typescript-ts-mode
  :config
  (setq typescript-indent-level 2
        typescript-ts-mode-indent-offset 2)
  :hook (typescript-ts-mode . prettier))

(use-package company
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)
  :hook (prog-mode . company-mode))

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

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package nix-mode
  :mode "\\.nix\\'"
  :functions nix-indent-line
  :hook (nix-mode . nixfmt-on-save-mode)
  :custom
  (nix-indent-function #'nix-indent-line))

(declare-function ivy-mode "ivy" ())
(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  (counsel-find-file-at-point t)
  :config (ivy-mode))

(setq counsel-find-file-at-point t)
(declare-function counsel-mode "counsel" ())
(use-package counsel
  :after ivy
  :diminish
  :config (counsel-mode))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package zeal-at-point
  :bind ("C-c d" . zeal-at-point))


;; (add-to-list 'load-path "/home/ryantm/.config/emacs/lisp/")
(use-package beancount
  :mode "\\.beancount\\'"
  :config (beancount-mode))



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
                    ("M-n" flymake-goto-next-error)
                    ("M-p" flymake-goto-prev-error)
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

(setq auth-sources '("~/.config/emacs/authinfo.gpg"))

(dir-locals-set-class-variables
 'huge-git-repository
 '((nil
    . ((magit-refresh-buffers . nil)
       (magit-revision-insert-related-refs . nil)))
   (magit-status-mode
      . ((eval . (magit-disable-section-inserter 'magit-insert-tags-header))
         (eval . (magit-disable-section-inserter 'magit-insert-recent-commits))
         (eval . (magit-disable-section-inserter 'magit-insert-unpushed-to-pushremote))
         (eval . (magit-disable-section-inserter 'magit-insert-unpushed-to-upstream-or-recent))
         (eval . (magit-disable-section-inserter 'magit-insert-unpulled-from-pushremote))
         (eval . (magit-disable-section-inserter 'magit-insert-unpulled-from-pushremote))
         (eval . (magit-disable-section-inserter 'magit-insert-unpulled-from-upstream))
         ))
))

(dir-locals-set-directory-class
   "/home/ryantm/p/nixpkgs/" 'huge-git-repository)

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
