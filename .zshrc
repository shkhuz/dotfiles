# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt extendedglob notify
unsetopt autocd beep nomatch
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/huzaifa/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

zle_highlight=(default:bold)

# Load version control information
autoload -Uz vcs_info
precmd() { vcs_info }

# Format the vcs_info_msg_0_ variable
zstyle ':vcs_info:git:*' formats '%F{green}%b%f'
 
# Set up the prompt (with git branch name)
setopt PROMPT_SUBST

# rehash completion
zshcache_time="$(date +%s%N)"

autoload -Uz add-zsh-hook

rehash_precmd() {
  if [[ -a /var/cache/zsh/pacman ]]; then
    local paccache_time="$(date -r /var/cache/zsh/pacman +%s%N)"
    if (( zshcache_time < paccache_time )); then
      rehash
      zshcache_time="$paccache_time"
    fi
  fi
}

add-zsh-hook -Uz precmd rehash_precmd

PROMPT='%B%F{white}%K{red}%n%f@%~%f%b%K{black}%F{red}█▓▒░%k%F{white}'
RPROMPT="\$vcs_info_msg_0_"
PATH="/home/huzaifa/.local/share/gem/ruby/2.7.0/bin:/home/huzaifa/.cargo/bin:/home/huzaifa/local/bin:/home/huzaifa/.local/bin:$PATH"
export PATH
export XDG_CONFIG_HOME=$HOME/.config/
alias ls='ls --color'
if type nvim > /dev/null 2>&1; then
	alias vim='nvim'
fi
