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

if [[ $(uname -s) == "Darwin" ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
    ensure_in_path PREFIX "/opt/homebrew/opt/grep/libexec/gnubin"
    ensure_in_path PREFIX "/opt/homebrew/opt/openjdk/bin"
    export LIBRARY_PATH="$LIBRARY_PATH:/opt/homebrew/lib/gcc/current:/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin24/14"
fi

if [ -d $HOME/.emacs.d/bin ]; then
    ensure_in_path PREFIX "$HOME/.emacs.d/bin"
fi

if [ -d $HOME/.local/bin ]; then
    ensure_in_path PREFIX "$HOME/.local/bin"
fi

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
if [[ $(uname -s) == "Darwin" ]]; then
    [[ -r "$(brew --prefix)/etc/profile.d/bash_completion.sh" ]] && . "$(brew --prefix)/etc/profile.d/bash_completion.sh"

    for bcfile in $(brew --prefix)/etc/bash_completion.d/* ; do
      . $bcfile
    done

    # MACOS settings
    export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES
    export HOMEBREW_NO_AUTO_UPDATE=1

    # Add tab completion for `defaults read|write NSGlobalDomain`
    # You could just use `-g` instead, but I like being explicit
    complete -W "NSGlobalDomain" defaults;

    # Add `killall` tab completion for common apps
    complete -o "nospace" -W "Contacts Calendar Dock Finder Mail Safari iTunes SystemUIServer Terminal Twitter" killall;
fi

# Enable tab completion for `g` by marking it as an alias for `git`
# if type _git &> /dev/null && [ -f $(brew --prefix)/etc/bash_completion.d/git-completion.bash ]; then
# 	complete -o default -o nospace -F _git g;
# fi;

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr ' ' '\n')" scp sftp ssh;

# The next line updates PATH for the Google Cloud SDK.
if [ -f "$HOME/Workspace/google-cloud-sdk/path.bash.inc" ]; then source "$HOME/Workspace/google-cloud-sdk/path.bash.inc"; fi

# The next line enables shell command completion for gcloud.
if [ -f "$HOME/Workspace/google-cloud-sdk/completion.bash.inc" ]; then source "$HOME/Workspace/google-cloud-sdk/completion.bash.inc"; fi

# minikub bash completion
# source ~/.minikube-completion

if [[ $(uname -s) == "Darwin" ]]
then
  export GOPATH="$HOME/Misc/go"
else
  export GOPATH="$HOME/go"
fi
ensure_in_path SUFFIX "$GOPATH/bin"

export LDAP_USERNAME="prashant"
export WORKSPACE="$HOME/Workspace"
export CLARISIGHTS_HOME="$WORKSPACE/repos/adwyze"

# alias for kubectl
source "$CLARISIGHTS_HOME/scripts/dev/kubectl_aliases.sh"

# alias for gssh
alias gssh=$WORKSPACE/repos/devops/scripts/gssh.sh

if [[ -z $SSH_TTY ]]
then
    export GPG_TTY=$(tty)
    gpg-connect-agent updatestartuptty /bye >/dev/null
    unset SSH_AGENT_PID
    if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
      export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
    fi
fi

# EMACS Settings
export LSP_USE_PLISTS=true

eval "$(rbenv init - bash)"
eval "$(fzf --bash)"

