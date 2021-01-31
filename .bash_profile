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
if [ -f $(brew --prefix)/etc/bash_completion ]; then
    source $(brew --prefix)/etc/bash_completion
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

# export PATH="/Users/prvithani/Workspace/graalvm-ce-1.0.0-rc5/Contents/Home/bin:$PATH:/usr/local/opt/imagemagick@6/bin"
export PATH="$PATH:/usr/local/opt/imagemagick@6/bin"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/prvithani/Workspace/google-cloud-sdk/path.bash.inc' ]; then source '/Users/prvithani/Workspace/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/prvithani/Workspace/google-cloud-sdk/completion.bash.inc' ]; then source '/Users/prvithani/Workspace/google-cloud-sdk/completion.bash.inc'; fi

# minikub bash completion
source ~/.minikube-completion

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# "-Xcompile.invokedynamic=true -Xfixnum.cache=false" for performance tuning
# "-Xcompile.invokedynamic=false -Xcompile.mode=OFF" in development mode only to speedup startup time OR "--dev" if JVMCI disabled (default behavior)

# ----- Prod JRUBY_OPTS ---- With Graal Compiler + InvokeDynamic + FixNum cache disabled ----
# export JRUBY_OPTS="-J-XX:+UnlockExperimentalVMOptions -J-XX:+EnableJVMCI -J-XX:+UseJVMCICompiler $(echo -J--add-opens=java.base/{java.lang,java.security,java.util,java.security.cert,java.util.zip,java.lang.reflect,java.util.regex,java.net,java.io,java.lang,sun.nio.ch,javax.crypto}=ALL-UNNAMED) -J--illegal-access=warn -Xcompile.invokedynamic=true -Xfixnum.cache=false"

# ----- Dev JRUBY_OPTS -----
# java-10
# export JRUBY_OPTS="$(echo -J--add-opens=java.base/{java.lang,java.security,java.util,java.security.cert,java.util.zip,java.lang.reflect,java.util.regex,java.net,java.io,java.lang,sun.nio.ch,javax.crypto,java.util.concurrent}=ALL-UNNAMED) -J--illegal-access=warn -J-Xmx2048m -J-Xms2048m -J-Xmn1024m -J-XX:ReservedCodeCacheSize=1024m -J-XX:+UseCodeCacheFlushing -J-XX:+UseG1GC -J-Xlog:gc:file=/tmp/gc.log -J-Djruby.thread.pool.enabled=true -J-Djruby.jit.threshold=1 -J-Djruby.compile.fastest=true --dev"

# java-8
export JRUBY_OPTS="-J-Xmx2048m -J-Xms2048m -J-Xmn1024m -J-XX:ReservedCodeCacheSize=1024m -J-XX:+UseCodeCacheFlushing -J-XX:+UseG1GC -J-Djruby.thread.pool.enabled=true -J-Djruby.jit.threshold=1 --dev"
export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_181.jdk/Contents/Home"

export EDITOR="nvim"

export LDAP_USERNAME="prashant"
export WORKSPACE="/Users/prvithani/Workspace"
export CLARISIGHTS_HOME="$WORKSPACE/adwyze"
export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"

export PATH="$PATH:/usr/local/opt/postgresql@10/bin"

# alias for kubectl
source "$CLARISIGHTS_HOME/scripts/dev/kubectl_aliases.sh"

# alias for gssh
alias gssh=$WORKSPACE/devops/scripts/gssh.sh

## MACOS settings
# export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES
# export HOMEBREW_NO_AUTO_UPDATE=1
