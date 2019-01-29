# Emacs Configuration

My emacs configuration. Includes some extras like powerline to make it a tad more vim-like, as well as some theme fiddling and custom settings. You will need a powerline patched font installed on the system (preferably Meslo S DZ) in order for this to work.

## To use

Install joker:

	brew install candid82/brew/joker 

Git clone this repo into `~/.emacs.d/`. Changes on top of the CSK (see below) primarily live in `init.el`.

You may also want to package install some things like `neo tree`, `git gutter` and `undo tree`; for a full list of packages installed, look in `package.el`.

You'll also need to install the silver searcher for js definition jumping to work.

    brew install ag

You might also want Tern:

    sudo npm install -g tern

## Notes

The initial setup for this configuration is based on the Emacs for Clojure Starter Kit, itself based on [The Emacs Starter Kit, v2](https://github.com/technomancy/emacs-starter-kit/tree/v2). Added functionality:

* Sets $PATH so that it's the same as your shell $PATH
* Includes the tomorrow-night and zenburn themes
* Turns off flyspell
* Adds some nrepl hooks, including auto-complete
* Prevents hippie-expand from expanding to file names
* Turns off ido-mode's use-file-name-at-point
* Stores backup files in `~/.saves`
* Installs the following packages by default:
    * starter-kit-lisp
    * starter-kit-bindings
    * starter-kit-ruby
    * clojure-mode
    * clojure-test-mode
    * nrepl
    * auto-complete
    * ac-nrepl

You can see all these tweaks in init.el and user.el
