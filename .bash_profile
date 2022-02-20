eval "$(/opt/homebrew/bin/brew shellenv)"

function ensure_in_path() {
  location="$1"
  segment="$2"
  if [[ "$PATH" != *"$segment"* ]]; then
    if [[ "$location" = "SUFFIX" ]]; then
      export PATH="$PATH:$segment"
    elif [[ "$location" = "PREFIX" ]]; then
      export PATH="$segment:$PATH"
    else
      echo -e "Syntax: ensure_in_path {SUFFIX|PREFIX} [SEGMENT]\nGOT: $@" >&2
    fi
  fi
}

ensure_in_path PREFIX "/opt/homebrew/opt/grep/libexec/gnubin"
ensure_in_path PREFIX "$HOME/.local/bin"
ensure_in_path PREFIX "$HOME/.emacs.d/bin"

[[ -f ~/.profile ]] && . ~/.profile

# Load the shell dotfiles, and then some:
# * ~/.path can be used to extend `$PATH`.
# * ~/.extra can be used for other settings you don't want to commit.
for file in ~/.{path,bash_prompt,exports,aliases,functions,extra}; do
	[ -r "$file" ] && [ -f "$file" ] && source "$file";
done;
unset file;

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob;

# Append to the Bash history file, rather than overwriting it
shopt -s histappend;

# Autocorrect typos in path names when using `cd`
shopt -s cdspell;

# Enable some Bash 4 features when possible:
# * `autocd`, e.g. `**/qux` will enter `./foo/bar/baz/qux`
# * Recursive globbing, e.g. `echo **/*.txt`
for option in autocd globstar; do
	shopt -s "$option" 2> /dev/null;
done;

# Add tab completion for many Bash commands
if [ -f $(brew --prefix)/profile.d/bash_completion ]; then
    source $(brew --prefix)/profile.d/bash_completion
fi

# Enable tab completion for `g` by marking it as an alias for `git`
# if type _git &> /dev/null && [ -f $(brew --prefix)/etc/bash_completion.d/git-completion.bash ]; then
# 	complete -o default -o nospace -F _git g;
# fi;

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr ' ' '\n')" scp sftp ssh;

# Add tab completion for `defaults read|write NSGlobalDomain`
# You could just use `-g` instead, but I like being explicit
complete -W "NSGlobalDomain" defaults;

# Add `killall` tab completion for common apps
complete -o "nospace" -W "Contacts Calendar Dock Finder Mail Safari iTunes SystemUIServer Terminal Twitter" killall;

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/prvithani/Workspace/google-cloud-sdk/path.bash.inc' ]; then source '/Users/prvithani/Workspace/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/prvithani/Workspace/google-cloud-sdk/completion.bash.inc' ]; then source '/Users/prvithani/Workspace/google-cloud-sdk/completion.bash.inc'; fi

# minikub bash completion
# source ~/.minikube-completion

export GOPATH="$HOME/Misc/go"
ensure_in_path SUFFIX "$GOPATH/bin"

export LDAP_USERNAME="prashant"
export WORKSPACE="/Users/prvithani/Workspace"
export CLARISIGHTS_HOME="$WORKSPACE/adwyze"

# alias for kubectl
# source "$CLARISIGHTS_HOME/scripts/dev/kubectl_aliases.sh"

# alias for gssh
alias gssh=$WORKSPACE/repos/devops/scripts/gssh.sh

export GPG_TTY=$(tty)
gpg-connect-agent updatestartuptty /bye >/dev/null
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
  export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi

# MACOS settings
export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES
export HOMEBREW_NO_AUTO_UPDATE=1

# EMACS Settings
export LSP_USE_PLISTS=true
