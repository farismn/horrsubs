{ nixpkgs ? import <nixpkgs> {}
, withHoogle
, input }:
if withHoogle
  then input.override (old: {
      overrides = nixpkgs.pkgs.lib.composeExtensions (old.overrides or (_:_: {}) (self: super: {
        ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
        ghcWithPackages = self.ghc.withPackages;
      }));
    })
  else input