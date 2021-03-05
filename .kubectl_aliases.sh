#@IgnoreInspection BashAddShebang
# Setup:
# 1. setup repo home if already not set(Add this line to your ~/.bashrc)
# export ADWYZE_REPO_HOME="<path to adwyze/adwyze repo>"
#
# 2. source this script in your ~/.bashrc (after you set repo home)
# source "$ADWYZE_REPO_HOME/scripts/dev/kubectl_aliases.sh"

# === Bash functions
# get bash shell on pod
function k8s_get_bash() {
    kubectl --context=$KUBECTL_CONTEXT --namespace=$KUBECTL_NAMESPACE exec -it $1 bash
}

# === Aliases
# aliases for kctl config functions
alias kns="kubens"
alias kctx="kubectx"

alias kctl="kubectl"
complete -F __start_kubectl kctl
# alias for kubectl
alias kl='kctl logs'
alias kp='kctl get pods --sort-by=.metadata.creationTimestamp'
alias kpr='kctl get pod --sort-by=.metadata.creationTimestamp --field-selector status.phase=Running'
# kpe will list pods that are not running or completed aka failed/errored pods
alias kpe='kctl get pod --sort-by=.metadata.creationTimestamp --field-selector status.phase=Failed'
alias kdp='kctl delete po'
alias pod='kctl get pods | grep -i'
alias ktop='kctl top pod'
alias kssh='k8s_get_bash'
