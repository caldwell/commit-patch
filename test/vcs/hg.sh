hg_init () {
    WD=$1
    mkdir -p "$WD"
    (cd "$WD" && hg init)

    export VC_DIFF="hg diff"
    export DIFF_PREFIX="a/"
}

hg_cleanup () {
    WD=$1
    rm -rf "$WD"
}
