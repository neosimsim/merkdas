with import <nixpkgs> {};

let
  ghc = haskell.packages.ghc881.ghcWithPackages (hP: with hP; [singletons zlib] );
in
derivation {
  name = "build";
  inherit ghc;
  hfmt = "${haskell.lib.dontCheck haskellPackages.hfmt}/bin/hfmt";
  builder = "${ghc}/bin/runghc";
  src = null;
  system = builtins.currentSystem;
}
