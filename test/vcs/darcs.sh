darcs_init () {
    WD=$1
    mkdir -p "$WD"
    (cd "$WD" && darcs init)

    export VC_DIFF="darcs diff"
    export VC_RM="true"
    export DIFF_PREFIX="a/"
}

darcs_cleanup () {
    WD=$1
    rm -rf "$WD"
}
