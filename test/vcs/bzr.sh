bzr_init () {
    WD=$1
    mkdir -p "$WD"
    (cd "$WD" && bzr init)

    export VC_DIFF="bzr diff"
    export VC_RM="true"
    export DIFF_PREFIX=""
}

bzr_cleanup () {
    WD=$1
    rm -rf "$WD"
}
