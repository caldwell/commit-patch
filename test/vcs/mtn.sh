mtn_init () {
    WD=$1
    REPO=$2

    export MTN_KEYDIR=$(mktemp -d "$TESTDIR_ABS/tmp/keydir.XXXXXXX")

    # Monotone doesns't like the db dir to exist when you init it.
    [ -d "$REPO" ] && rmdir "$REPO"

    mtn --keydir "$MTN_KEYDIR" db init --db="$REPO"
    mtn --keydir "$MTN_KEYDIR" --db="$REPO" --branch=test setup "$WD"
    (cd "$WD" && mtn automate generate_key commit-patch-automated-tester@example.com '')

    export VC_DIFF="mtn diff --without-header"
    export VC_RM="mtn drop"
    export DIFF_PREFIX=""
}

mtn_cleanup () {
    WD=$1
    REPO=$2
    rm -rf "$REPO"
    rm -rf "$WD"
    rm -rf "$MTN_KEYDIR"
}
