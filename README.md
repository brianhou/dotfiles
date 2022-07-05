# dotfiles

``` bash
$ git clone git@github.com:brianhou/dotfiles.git
$ ./dotfiles/install_symlinks.sh
```

## Emacs

Install Doom Emacs, which requires Emacs 27.

### [macOS][doom-emacs-install-macos]

```bash
$ brew install git ripgrep coreutils fd
$ brew tap railwaycat/emacsmacport
$ brew install emacs-mac --with-modules --with-starter
$ ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications/Emacs.app
```

### [Ubuntu][doom-emacs-install-ubuntu]

``` bash
$ sudo apt add-apt-repository ppa:kelleyk/emacs
$ sudo apt update
$ sudo apt install ripgrep fd-find emacs27
```

[doom-emacs-install-macos]: https://github.com/hlissner/doom-emacs/blob/master/docs/getting_started.org#with-homebrew
[doom-emacs-install-ubuntu]: https://github.com/hlissner/doom-emacs/blob/master/docs/getting_started.org#ubuntu

### Doom Emacs

``` bash
$ git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
$ doom install
```

## Python

Install [pyenv][pyenv-install] and [pyenv-virtualenv][pyenv-virtualenv-install].

[pyenv-install]: https://github.com/pyenv/pyenv#installation
[pyenv-virtualenv-install]: https://github.com/pyenv/pyenv-virtualenv#installation
