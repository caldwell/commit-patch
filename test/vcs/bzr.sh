bzr_init () {
    WD=$1
    mkdir -p "$WD"
    (cd "$WD" && bzr init && bzr whoami --branch "Chester McTester <chester@example.com>")

    export VC_DIFF="bzr diff"
    export VC_RM="true"
    export DIFF_PREFIX=""
}

bzr_cleanup () {
    true
}
