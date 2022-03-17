#!/usr/bin/env bash

set -ex

dotfiles=$(dirname "$0")

ln -s ${dotfiles}/.doom.d $HOME/.doom.d

ln -s ${dotfiles}/.bashrc $HOME/.bashrc
