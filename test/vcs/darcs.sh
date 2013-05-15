darcs_init () {
    WD=$1
    mkdir -p "$WD"
    (cd "$WD" && darcs init)

    export VC_DIFF="darcs diff"
    export DIFF_PREFIX="a/"
}

darcs_cleanup () {
    WD=$1
    rm -rf "$WD"
}
