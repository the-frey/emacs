# ~/.emacs.d

> This is my dotemacs. There are many like it, but this one is mine.

> My dotemacs is my best friend. It is my life. I must master it as I must master my life.

> Without me, my dotemacs is useless. Without my dotemacs, I am useless. I must hit my keybindings true. I must deliver faster than my competitor who is trying to beat me. I must beat them before they beat me.

## Emacs Configuration

My emacs configuration. Includes some extras like powerline to make it a tad more vim-like, as well as some theme fiddling and custom settings. You will need a powerline patched font installed on the system (preferably Meslo S DZ, although Ubuntu Mono will work as well) in order for this to work.

## Cyberpunk

Currently, I'm working on, and using the `cyberpunk_overhaul` branch. It's a complete overhaul of the Tron Legacy emacs theme based on the garish Cyberpunk themes for iTerm and VS Code. Eventually, I'll probably break it out into a proper theme of its own.

![cyberpunk](https://raw.githubusercontent.com/the-frey/emacs/cyberpunk_overhaul/cyberpunk-emacs.png)

## To use

Install joker:

	brew install candid82/brew/joker 

Git clone this repo into `~/.emacs.d/`. Changes on top of the CSK (see below) primarily live in `init.el`.

You may also want to package install some things like `neo tree`, `git gutter` and `undo tree`; for a full list of packages installed, look in `package.el`.

You'll also need to install the silver searcher for js definition jumping to work.

    brew install ag

You might also want Tern:

    sudo npm install -g tern

For Indium (JS REPLs) you will need:

    npm install -g indium

You might need fonts:

    M-x all-the-icons-install-fonts

Depending on your emacs version, you'll probably need to `M-x package-refresh-contents` and restart in order to have decent defaults.

To get all the go stuff working, you'll need:

```sh
go get -u github.com/mdempsky/gocode
go get github.com/rogpeppe/godef
go get -u github.com/dougm/goflymake
go get golang.org/x/tools/cmd/goimports
go get golang.org/x/tools/cmd/godoc
go get -u golang.org/x/lint/golint
```

As well as a working go installation and gopath.

For Rust, you'll need to install `rustup` and then:

    rustup component add rust-src

```sh
git clone https://github.com/rust-analyzer/rust-analyzer.git
cd rust-analyzer
cargo xtask install --server
```

## Terminal theme

There's also a Terminal theme in this folder, loosely based on the Cyberounk theme. It's compatible with iTerm2, if that's your bag.

## Notes

The initial setup for this configuration is based on the Emacs for Clojure Starter Kit, itself based on [The Emacs Starter Kit, v2](https://github.com/technomancy/emacs-starter-kit/tree/v2).
