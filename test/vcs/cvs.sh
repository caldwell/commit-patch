cvs_init () {
    WD=$1
    REPO=$2

    export CVSROOT="$REPO"
    cvs init
    mkdir "$REPO/test"
    cvs checkout -d "$WD" test

    export VC_DIFF="cvs diff -N"
    export DIFF_PREFIX=""
}

cvs_cleanup () {
    WD=$1
    REPO=$2
    rm -rf "$WD"
    rm -rf "$REPO"
}
