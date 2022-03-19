#!/usr/bin/env bash

set -ex

dotfiles=$(dirname "$0")

ln -s `realpath ${dotfiles}/.doom.d` $HOME/.doom.d

mv $HOME/.bashrc $HOME/.bashrc.bak
ln -s `realpath ${dotfiles}/.bashrc` $HOME/.bashrc
