#!/bin/sh

red() {
  echo " $(tput setaf 1)$*$(tput setaf 9)"
}

yellow() {
  echo " $(tput setaf 3)$*$(tput setaf 9)"
}

green() {
  echo " $(tput setaf 2)$*$(tput setaf 9)"
}

link_dotfile() {
  source="$1"
  dest="$2"
  full_dest="$HOME/$dest"
  if [ -e "$full_dest" ] || [ -L "$full_dest" ]; then
    if [ -L "$full_dest" ] && [ "$full_dest" -ef "$source" ]; then
      yellow "✔ $dest is already linked"
    else
      red "✘ $dest already exists"
    fi
  else
    ln -s "$(pwd)/$source" "$full_dest"
    green "✔ $dest has been linked"
  fi
}

mkdir ~/.emacs.d/

link_dotfile "zsh/zshrc" ".zshrc"
link_dotfile "zsh/aliases" ".aliases"
link_dotfile "tmux/tmux" ".tmux"
link_dotfile "tmux/tmux.conf" ".tmux.conf"
link_dotfile "vim/vim" ".vim"
link_dotfile "vim/vimrc" ".vimrc"
link_dotfile "emacs/.emacs.d/init.el" ".emacs.d/init.el"
