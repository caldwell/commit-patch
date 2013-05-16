git_init () {
    WD=$1
    mkdir -p "$WD"
    (cd "$WD" && git init)

    export VC_DIFF="git diff"
    export VC_RM="true"
    export DIFF_PREFIX="b/"
}

git_cleanup () {
    WD=$1
    rm -rf "$WD"
}
