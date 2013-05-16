svn_init () {
    WD=$1
    REPO=$2
    set -e

    (cd "$REPO" && svnadmin create test)
    svn import "$WD" file://"$REPO"/test/trunk -m "Setting up trunk"
    svn checkout file://"$REPO"/test/trunk "$WD"

    export VC_DIFF="svn diff"
    export VC_RM="svn rm"
    export DIFF_PREFIX=""
}

svn_cleanup () {
    true
}
