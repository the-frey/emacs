(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (Package-refresh-contents))

;; vim style powerline and custom theming
(add-to-list 'load-path "~/.emacs.d/vendor/powerline")
(require 'powerline)
(powerline-default-theme)
(setq powerline-default-separator 'utf-8)
(custom-set-faces
   '(mode-line ((t (:foreground "#fafafa" :background "DarkOrange" :box nil))))
   '(mode-line-inactive ((t (:foreground "#fafafa" :background "#666666" :box nil)))))

;; syntax highlighting for cider repl
(setq cider-repl-use-clojure-font-lock t)

;; ace jump mode
(add-to-list 'load-path "~/.emacs.d/vendor/ace-jump-mode.el")
(autoload
    'ace-jump-mode
    "ace-jump-mode"
    "Emacs quick move minor mode"
    t)
;; set key binding to C-c <spc>
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; we need to be able to type M-3 to get a hash
(fset 'insertHash "#")
(global-set-key (kbd "M-3") 'insertHash)

;; relative line numbers ftw
(add-to-list 'load-path "~/.emacs.d/linum-relative")
(require 'linum-relative)

;; git modeline
(add-to-list 'load-path "~/.emacs.d/vendor/git-modeline.el")

;; set up neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; load user file from starter kit
(load "~/.emacs.d/user.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   [zenburn-bg zenburn-red zenburn-green zenburn-yellow zenburn-blue zenburn-magenta zenburn-cyan zenburn-fg])
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (Deviant)))
 '(custom-safe-themes
   (quote
    ("9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "b2492bc021874b54513443587d9c173107fa5a6ca0480d45631e4db72f9eea26" default)))
 '(fci-rule-color "#2a2a2a")
 '(global-linum-mode t)
 '(menu-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2e3735" :foreground "#b6beb4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Meslo LG S DZ for Powerline")))))
