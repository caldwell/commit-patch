with import <nixpkgs>{};
with { commit-patch' = import ./commit-patch.nix; };

{ commit-patch ? commit-patch'
,  emacs ? pkgs.emacs-nox
}:

let
  trivialBuild = pkgs.callPackage "${<nixpkgs>}/pkgs/build-support/emacs/trivial.nix" { emacs = emacs; };
in

trivialBuild {
  pname = "commit-patch-buffer";
  inherit (commit-patch) src version;

  propagatedUserEnvPkgs = [ commit-patch patchutils ];

  meta = {
    inherit (commit-patch.meta) homepage license;
    description = "Commit diff buffers using commit-patch";
  };
}
