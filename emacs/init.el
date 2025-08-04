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

;; Enable Vertico.
(use-package vertico
  ;;:custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Corfu

  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package corfu
  ;; TAB-and-Go customizations
  :custom
  (corfu-cycle t)           ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect 'prompt) ;; Always preselect the prompt

  (corfu-auto t)
  (corfu-quit-no-match 'separator)

  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  :init
  (global-corfu-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-s" . consult-line)
         ("C-r" . consult-line)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)


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
                    ("C-x C-f" find-file-at-point)
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
