(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)

(require 'package)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(helm helm-ag projectile rainbow-mode undo-tree company-tern all-the-icons exec-path-from-shell js2-mode rjsx-mode xref-js2 git-gutter git-gutter-fringe multiple-cursors cyberpunk-theme material-theme starter-kit starter-kit-bindings starter-kit-lisp cider robe flymake-ruby company robe powerline neotree flycheck rainbow-delimiters)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; split window into 3 to get cracking
(defun split-3-windows-horizontally-evenly ()
  (interactive)
  (command-execute 'split-window-horizontally)
  (command-execute 'split-window-horizontally)
  (command-execute 'balance-windows)
)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; add joker and flycheck
(add-to-list 'load-path "~/.emacs.d/vendor/flycheck-joker")
(require 'flycheck-joker)
(autoload
    'flycheck-mode
    "flycheck-mode"
    "Flycheck mode - includes joker for clj"
    t)
(add-hook 'clojure-mode-hook 'flycheck-mode)

;; js and jsx
(setq js2-strict-missing-semi-warning nil)
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(require 'company-tern)
(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)
                           (electric-indent-local-mode -1)))
(setq js2-mode-hook
  '(lambda () (progn
    (set-variable 'indent-tabs-mode nil))))
                           
;; Disable completion keybindings, as we use xref-js2 instead
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

;; disable auto fill as js2 calls it directly
(defun my-fill-nobreak-predicate ()
  (not (nth 4 (syntax-ppss))))

(defun my-prog-auto-fill ()
  (setq-local fill-nobreak-predicate #'my-fill-nobreak-predicate)
  (auto-fill-mode 1))

(add-hook 'prog-mode-hook #'my-prog-auto-fill)

;; cljs
(setq cider-cljs-lein-repl
	"(do (require 'figwheel-sidecar.repl-api)
         (figwheel-sidecar.repl-api/start-figwheel!)
         (figwheel-sidecar.repl-api/cljs-repl))")

;;(desktop-save-mode)
(global-set-key (kbd "C-x 4") 'split-3-windows-horizontally-evenly)

;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

;; make sure eldoc is on
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
;;(global-eldoc-mode t)

;; ruby flymake
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; robe for ruby
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

;; company mode, with robe
(global-company-mode t)
(push 'company-robe company-backends)

;; no scrollbars, thanks
(scroll-bar-mode -1)

;; no tabs, thanks
(setq-default indent-tabs-mode nil)

;; vim style powerline and custom theming
(add-to-list 'load-path "~/.emacs.d/vendor/powerline")
(require 'powerline)
(powerline-default-theme)
(setq powerline-default-separator 'utf-8)

;; custom theming from M-x customize
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Meslo LG S DZ for Powerline")))))

;; we want to make sure cider can find lein
(add-to-list 'exec-path "/usr/local/bin") 

;; make the mouse less insane
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

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

;; we want to be able to insert cursors at beginning and end of lines
(global-set-key (kbd "C-<") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C->") 'mc/edit-ends-of-lines)

;; we want to be able to mark next like this
(global-set-key (kbd "C-c C-d") 'mc/mark-next-like-this)

;; we want to be able to mark all like this
(global-set-key (kbd "C-*") 'mc/mark-all-like-this)

;; control shift click to place multiple cursors
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; relative line numbers ftw
(add-to-list 'load-path "~/.emacs.d/linum-relative")
(require 'linum-relative)

;; git modeline
(add-to-list 'load-path "~/.emacs.d/vendor/git-modeline.el")

;; pretty clojure lambdas and such
(eval-after-load 'clojure-mode
 '(font-lock-add-keywords
   'clojure-mode `(("\\(#\\)("
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "λ")
                              nil))))))

;; sensible margins
(defun my-set-margins ()
  "Set margins in current buffer."
  (setq left-margin-width 24))

;; git gutter
(require 'git-gutter-fringe)

;; some extra stuff for the cider repl
;; which will make it doubleplusawesome

;; enable eldoc in clojure buffers
(add-hook 'cider-mode-hook 'eldoc-mode)
;; show port name 
(setq nrepl-buffer-name-show-port t)
;; clojure syntax highlighting
(setq cider-repl-use-clojure-font-lock t)
;; set result prefix
(setq cider-repl-result-prefix ";; => ")
(setq cider-interactive-eval-result-prefix ";; => ")
;; disable auto selection of the error buffer
(setq cider-auto-select-error-buffer nil)
;; wrap errors
(setq cider-stacktrace-fill-column t)
;; enable paredit
(add-hook 'cider-repl-mode-hook 'paredit-mode)
;; bind F9 to the cider test runner
(global-set-key [f9] 'cider-test-run-tests)
;; rainbow parens
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; margins
;; (add-hook 'prog-mode-hook 'my-set-margins)

;; set up neotree and icons
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(require 'all-the-icons)
(setq neo-theme 'icons)

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
 '(custom-safe-themes
   (quote
    ("efefb69e7781fcfe62f3d0b573701f56e45e29afbe9e378a422025fd767ea246" "addfaf4c6f76ef957189d86b1515e9cf9fcd603ab6da795b82b79830eed0b284" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "b2492bc021874b54513443587d9c173107fa5a6ca0480d45631e4db72f9eea26" default)))
 '(fci-rule-color "#2a2a2a")
 '(global-linum-mode t)
 '(initial-buffer-choice t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (helm-ag helm package-lint projectile rainbow-mode undo-tree dash-functional company-tern xref-js2 rjsx-mode js2-mode tron-theme multiple-cursors cyberpunk-theme material-theme exec-path-from-shell flycheck-joker rainbow-delimiters starter-kit-lisp starter-kit-bindings robe powerline neotree git-gutter-fringe flymake-ruby company cider)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

;; widen fringe
(fringe-mode '(20 . 0))

;; evidence of my indecision over themes

;; (load-theme 'flatland t)
;; (load-theme 'material t)
;; (load-theme 'misterioso t)
(load-theme 'cyberpunk-2019 t)

;; flycheck styling
(set-face-attribute 'flycheck-error nil :underline '(:color "#FF4081"))
(set-face-attribute 'flycheck-warning nil :underline '(:color "#FF9C00"))
(set-face-attribute 'flycheck-info nil :underline '(:color "#9C00FF"))


