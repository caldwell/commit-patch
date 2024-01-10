with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "commit-patch";
  version = "2.6.2";
  src = fetchurl {
    url    = "https://porkrind.org/commit-patch/commit-patch-${version}.tar.gz";
    sha256 = "0v11vjyisk243zi0ym90bnqb229j7iaqx1lwqdkszxzn1yxwq4ck";
  };

  buildInputs = [ perl patchutils makeWrapper ];

  phases = "unpackPhase installPhase postFixup";

  installFlags = [ "PREFIX=$(out)" ];

  # If it weren't fatpacked we could do this for IPC::Run:
  # postFixup = ''
  #   wrapProgram $out/bin/commit-patch --set PERL5LIB \
  #   ${with perlPackages; makePerlPath ([
  #     IPCRun
  #   ])} \
  #   --prefix PATH ":" \
  #   "${lib.makeBinPath [ perl patchutils ]}"
  # '';

  postFixup = ''
    perl -pi -e 's,#!/usr/bin/perl,#!${perl}/bin/perl,' $out/bin/commit-patch
    wrapProgram $out/bin/commit-patch \
    --prefix PATH ":" \
    "${lib.makeBinPath [ perl patchutils ]}"
  '';

  meta = with lib; {
    license = licenses.gpl2;
    homepage = "https://porkrind.org/commit-patch";
    description = "Commit patches to Darcs, Git, Mercurial, Bazaar, Monotone, Subversion, or CVS";
  };
}
