;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Packages
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(if nil
    (progn
      (setq my-onlinep nil)
      (unless
          (condition-case nil
              (delete-process
               (make-network-process
                :name "my-check-internet"
                :host "elpa.gnu.org"
                :service 80))
            (error t))
        (setq my-onlinep t))

      (setq my-packages
            '(dash
              expand-region
              magit
              multiple-cursors
              popwin
              projectile
              s
              smex
              wrap-region
              yasnippet
              diminish
              auto-complete
              clojure-mode
              clojure-test-mode
              cider
              markdown-mode
              haskell-mode
              yaml-mode
              rainbow-delimiters
              paredit
              feature-mode
              cus-edit+
              multi-term
              rvm))

      (when my-onlinep
        (package-refresh-contents)
        (dolist (p my-packages)
          (unless (package-installed-p p)
            (package-install p))))))

;; Customizations
(setq custom-file "~/.emacs.d/custom-file.el")
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

;; ----------
;; -- Loading Modules
;; ----------
(defun autoload-mode (name regex &optional file)
  "Automatically loads a language mode
when opening a file of the appropriate type.

`name' is the name of the mode.
E.g. for javascript-mode, `name' would be \"javascript\".

`regex' is the regular expression matching filenames of the appropriate type.

`file' is the name of the file
from which the mode function should be loaded.
By default, it's `name'-mode.el."
  (let* ((name-mode (concat name "-mode"))
         (name-sym (intern name-mode)))
    (autoload name-sym (or file name-mode)
      (format "Major mode for editing %s." name) t)
    (add-to-list 'auto-mode-alist (cons regex name-sym))))

(autoload-mode "tex" "\\.tex$" "auctex")
(autoload-mode "d" "\\.d[i]?\\'$")
(autoload-mode "textile" "\\.textile$")
(autoload-mode "haml" "\\.haml$")
(autoload-mode "sass" "\\.sass$")
(autoload-mode "rhtml" "\\.\\(rhtml\\|erb\\)$")
(autoload-mode "yaml" "\\.ya?ml$")
(autoload-mode "ruby" "\\(\\.\\(rb\\|rake\\|rjs\\|gemspec\\|thor\\|rhtml\\)\\|Rakefile\\|Capfile\\|Thorfile\\|Gemfile\\|Guardfile\\)$")
(autoload-mode "css" "\\.css$")
(autoload-mode "arc" "\\.arc$" "arc")
(autoload-mode "erlang" "\\.\\([he]rl\\|yaws\\)$" "erlang/erlang")
(autoload-mode "treetop" "\\.treetop$")
(autoload-mode "factor" "\\.factor$")
(autoload-mode "actionscript" "\\.as$")
(autoload-mode "xml" "\\.\\(swfml\\|xml\\)$")
(autoload-mode "org" "\\.org$")
(autoload-mode "haxe" "\\.hx$")

(eval-after-load "ruby-mode"
  '(progn
     (define-key ruby-mode-map (kbd "C-M-l") 'ruby-forward-sexp)
     (define-key ruby-mode-map (kbd "C-M-j") 'ruby-backward-sexp)
     (setq ruby-deep-indent-paren-style nil)
     (define-key ruby-mode-map (kbd "C-x e") 'ruby-send-whole-buffer)
     (define-key ruby-mode-map (kbd "C-x C-e") 'ruby-send-whole-buffer)
     (defun ruby-send-whole-buffer ()
       (interactive)
       (save-buffer)
       (ruby-load-file (buffer-file-name (current-buffer))))))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (paredit-mode +1)
                                  (rainbow-delimiters-mode +1)))
(add-hook 'clojure-mode-hook (lambda ()
                                  (paredit-mode +1)
                                  (rainbow-delimiters-mode +1)))


;;(autoload 'ghc-init "ghc" nil t)
;;(autoload 'ghc-debug "ghc" nil t)
;;(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;; in text mode do spellchecking
(add-hook 'text-mode-hook 'flyspell-mode)

(eval-after-load "sql"
  '(progn
     (sql-set-product 'mysql)))

(setq enable-local-variables :safe)

;; Tabs
(setq indent 2)
(setq js-indent-level 2)
(add-hook 'shell-mode-hook
          (lambda ()
            (setq tab-width 8)))

;; Expand region
;(require 'expand-region)
;(global-set-key (kbd "C-@") 'er/expand-region)


;; Appearance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-font-lock-mode t)
(setq inhibit-splash-screen t)

(do-rebindings my-rebinds)

;; Require libraries for use in initializations
(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))

;; Diminish removes modes from your mode line
(when-available 'diminish
                (progn
                  (eval-after-load 'whitespace-mode '(diminish 'whitespace-mode))
                  (eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))))

;; Fullscreen magit-status
(when-available 'magit-svn
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

;; Colors and fonts
(set-frame-parameter nil 'font-backend "xft")
(set-default-font "Source Code Pro:size=22")

(require 'rvm)
(rvm-use-default)
