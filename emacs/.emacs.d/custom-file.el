(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms
   (quote
    (("\\\\`/[^/]*:\\\\([^/]*/\\\\)*\\\\([^/]*\\\\)\\\\'" "/tmp/\\\\2" t))))
 '(backup-directory-alist (quote (("." . "/home/ryantm/.emacs.d/backups/"))))
 '(column-number-mode t)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(custom-enabled-themes (quote (sanityinc-solarized-light)))
 '(custom-file "~/.emacs.d/custom-file.el")
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(enable-local-variables :safe)
 '(fci-rule-color "#073642")
 '(fci-rule-column 80)
 '(git-commit-summary-max-length 80)
 '(haskell-indent-offset 2)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen nil)
 '(initial-major-mode (quote fundamental-mode))
 '(initial-scratch-message nil)
 '(keyboard-coding-system (quote utf-8-unix))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (elisp-slime-nav rainbow-delimiters paredit zeal-at-point yaml-mode use-package spu powerline nix-mode multiple-cursors markdown-preview-mode magit ledger-mode inf-ruby hi2 haml-mode flycheck-haskell fill-column-indicator cus-edit+ color-theme-sanityinc-solarized bash-completion)))
 '(save-place t nil (saveplace))
 '(save-place-file "/home/ryantm/.emacs.d/.places")
 '(scroll-bar-mode nil)
 '(selection-coding-system (quote utf-8))
 '(send-mail-function (quote mailclient-send-it))
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil)
 '(vc-follow-symlinks t)
 '(vc-make-backup-files t)
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background nil :foreground nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "adobe" :family "Source Code Pro"))))
 '(ledger-font-xact-highlight-face ((t nil)) t)
 '(whitespace-newline ((t (:foreground "gray35" :weight ultra-light))))
 '(whitespace-space ((t (:foreground "grey35" :weight ultra-light)))))
