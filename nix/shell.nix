let
  nixpkgs = import ./nixpkgs.nix {};
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;
  pkgDrv = haskellPackages.callPackage ./haskell/default.nix {};
  haskellDeps = pkgDrv.getBuildInputs.haskellBuildInputs;
  ghc = haskellPackages.ghcWithHoogle (_: haskellDeps);
in
  pkgs.mkShell {
    name = "rafflesia-shell";
    buildInputs = with haskellPackages; [
      ghc
      cabal-install
      hpack
      hlint
      stylish-haskell
      ghcid
    ];
  }