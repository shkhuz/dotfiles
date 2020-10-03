# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/huzaifa/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# huzaifa's config
alias ls="ls -F --color=auto"
alias zig="~/binary-files/zig/zig"
alias caps="xmodmap ~/.xmodmap"
alias uth="~/scripts/usb_tethering.sh 2> /dev/null"
alias commit="git a && git c"
export PROMPT='%F{yellow}%B%n@%M%b%f %~ # '
export BROWSER="vimb"
export PATH="/home/huzaifa/Downloads/node-v12.18.3-linux-x64/bin/:/opt/cross/bin:~/scripts/:~/projects/editor/:$PATH"
