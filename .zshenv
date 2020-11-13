function ensure_in_path() {
  location="$1"
  segment="$2"
  setopt localoptions nonomatch
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
# LLVM
ensure_in_path PREFIX "/usr/local/opt/llvm/bin"
# If you come from bash you might have to change your $PATH.
ensure_in_path PREFIX "$HOME/bin:/usr/local/bin"

# Add postgresql path
ensure_in_path PREFIX "$HOME/postgresql/bin"

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
ensure_in_path SUFFIX "$HOME/.rvm/bin"

# export PATH="/Users/prvithani/Workspace/graalvm-ce-1.0.0-rc5/Contents/Home/bin:$PATH:/usr/local/opt/imagemagick@6/bin"
ensure_in_path SUFFIX "/usr/local/opt/imagemagick@6/bin"

ensure_in_path SUFFIX "$GOPATH/bin"

ensure_in_path SUFFIX "/usr/local/opt/mongodb-community@3.6/bin"
ensure_in_path SUFFIX "$(brew --prefix openvpn)/sbin"
ensure_in_path SUFFIX "$HOME/.emacs.d/bin"
