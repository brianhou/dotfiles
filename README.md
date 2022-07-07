# dotfiles

``` bash
$ git clone git@github.com:brianhou/dotfiles.git
```

## macOS Setup

1.  Install xcode command-line tools.

    ``` bash
    $ chsh -s /bin/bash       # optional: set bash as default terminal
    $ xcode-select --install  # install xcode command-line tools
    ```

2.  Install [iTerm2](https://iterm2.com/), with these [preferences](#).

3.  Create an SSH key and [register with GitHub](https://github.com/settings/keys).

4.  Install [Homebrew](https://brew.sh/).

    ``` bash
    $ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    ```

5.  Clone this repository and install symlinks to the desired dotfiles.

    ```bash
    $ ln -s ~/Development/dotfiles/minimal.bashrc ~/.bashrc
    $ ln -s ~/Development/dotfiles/minimal.bash_profile ~/.bash_profile
    $ ln -s ~/Development/dotfiles/minimal.profile ~/.profile
    $ ln -s ~/Development/dotfiles/minimal.vimrc ~/.vimrc
    $ ln -s ~/Development/dotfiles/.doom.d/ ~/.doom.d
    $ ln -s ~/Development/dotfiles/.gitconfig ~/.gitconfig
    $ ln -s ~/Development/dotfiles/.gitignore_global ~/.gitignore_global
    ```

6.  Install [Doom Emacs][install-doom-emacs-macos], which requires Emacs 27.

    ```bash
    $ brew install git ripgrep coreutils fd
    $ brew tap railwaycat/emacsmacport
    $ brew install emacs-mac --with-modules --with-starter
    $ ln -s /opt/homebrew/Cellar/emacs-mac/<version>/Emacs.app /Applications/Emacs.app
    $ git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
    $ ~/.emacs.d/bin/doom install
    ```

7.  Install [pyenv][install-pyenv] and [pyenv-virtualenv][install-pyenv-virtualenv].

    ``` bash
    $ brew install pyenv pyenv-virtualenv
    $ brew install openssl readline sqlite3 xz zlib tcl-tk
    $ pyenv install <version>
    ```


## Ubuntu Setup

1.  Install [Doom Emacs][install-doom-emacs-ubuntu], which requires Emacs 27.

    ``` bash
    $ sudo apt add-apt-repository ppa:kelleyk/emacs
    $ sudo apt update
    $ sudo apt install ripgrep fd-find emacs27
    $ git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
    $ ~/.emacs.d/bin/doom install
    ```

2.  Install [pyenv][install-pyenv] and [pyenv-virtualenv][install-pyenv-virtualenv].


## Miscellaneous Tools

- Install [Docker][install-docker].



[install-doom-emacs-macos]: https://github.com/hlissner/doom-emacs/blob/master/docs/getting_started.org#with-homebrew
[install-doom-emacs-ubuntu]: https://github.com/hlissner/doom-emacs/blob/master/docs/getting_started.org#ubuntu
[install-pyenv]: https://github.com/pyenv/pyenv#installation
[install-pyenv-virtualenv]: https://github.com/pyenv/pyenv-virtualenv#installation
[install-docker]: https://www.docker.com/get-started
