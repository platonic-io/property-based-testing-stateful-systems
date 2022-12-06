let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.11.tar.gz";
    sha256 = "11w3wn2yjhaa5pv20gbfbirvjq6i3m7pqrq2msf0g7cv44vijwgw";
  }) {};
in
  with pkgs;

  mkShell rec {
    buildInputs = [
      act
      cabal-install
      haskell.compiler.ghc925
      haskellPackages.cabal-fmt
      haskellPackages.ormolu
      haskellPackages.tasty-discover
      hlint
      gmp
      pandoc
      pkgconfig
      stylish-haskell
      zlib
    ];

    # Ensure that libz.so and other libraries are available to TH splices, cabal
    # repl, etc.
    LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
  }
