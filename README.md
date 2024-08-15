# My dotfiles

## Project

This project is to get me up and running quickly on any new machine, but also keep my dotfiles in sync across multiple machines.

It is probably of little use to other folk, but there may be things in here you can draw inspiration from in the same way I have from others that have published their dotfiles.

## Usage

Clone this repo
`git submodule update -i`

Use "stow" to link the packages (https://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html)
```sh
stow vim
stow zsh
```
## Environment

Tested on MacOS and various Linux distros.

Assumes a set up of zsh, vim, & tmux.
