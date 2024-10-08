# check dependency
dependency_exists() {
    type "$1" > /dev/null 2>&1
}

# Use oh-my-zsh
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="af-magic"

# Enable oh-my-zsh plugins
plugins=(git)

# Init oh-my-zsh
source $ZSH/oh-my-zsh.sh

# HSTR configuration
export HISTFILE=~/.zsh_history              # set the history file

if dependency_exists hstr; then
  setopt histignorespace                      # skip cmds w/ leading space from history
  export HSTR_CONFIG=prompt-bottom,hide-help,raw-history-view,hicolor
  bindkey -s "\C-r" "\C-a hstr -- \C-j"       # bind hstr to Ctrl-r (for Vi mode check doc)
fi

# Add aliases
if [ -f ~/.zsh_aliases ]; then
  source ~/.zsh_aliases
fi

# Add env secrets
if [ -f ~/.env_private ]; then
  source ~/.env_private
fi
