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

(defvar my-packages '(starter-kit starter-kit-bindings starter-kit-lisp cider robe flymake-ruby company robe powerline neotree)
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

;; Display ido results vertically, rather than horizontally
; (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
; (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
; (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
;   (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
;     (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;     (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
; (add-hook 'ido-setup-hook 'ido-define-keys)

;; vim style powerline and custom theming
(add-to-list 'load-path "~/.emacs.d/vendor/powerline")
(require 'powerline)
(powerline-default-theme)
(setq powerline-default-separator 'utf-8)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2e3735" :foreground "#b6beb4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Meslo LG S DZ for Powerline"))))
 '(mode-line ((t (:foreground "#fafafa" :background "DarkOrange" :box nil))))
 '(mode-line-inactive ((t (:foreground "#fafafa" :background "#666666" :box nil)))))

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
(global-set-key (kbd "C-S-n") 'mc/mark-next-like-this)

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
 '(initial-buffer-choice t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (rainbow-delimiters starter-kit-lisp starter-kit-bindings robe powerline neotree git-gutter-fringe flymake-ruby company cider)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

;;(load-theme 'flatland t)

