# My dotfiles

## Project

This project is to get me up and running quickly on any new machine, but also keep my dotfiles in sync across multiple machines.

It is probably of little use to other folk, but there may be things in here you can draw inspiration from in the same way I have from others that have published their dotfiles.

## Usage

1. Clone this repo:

    ```sh
    git clone git@github.com:dazjones/dotfiles.git
    ```

2. Update/install submodules:

    ```sh 
    git submodule update -i
    ```

3. I use [stow](https://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html) to link packages, so install using the package manager of your choice.

    ```sh
    sudo apt install stow
    sudo dnf install stow
    brew install stow
    ```

4. Link required packages using stow ensuring target directory is user home.

    ```sh
    stow -t ${HOME} vim
    stow -t ${HOME} zsh
    ```

    You can also use the `-v` flag to increase verbosity and get some sense of what it is doing:

    ```
    stow -vv -t ${HOME} package_name
    ```
## Environment

Tested on MacOS and various Linux distros.
