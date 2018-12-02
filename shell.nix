{ nixpkgs ? import <nixpkgs> {} }:
let
  env = (import ./default.nix {inherit nixpkgs;}).env.overrideAttrs (oldAttrs: { buildInputs = oldAttrs.buildInputs ++ [nixpkgs.haskellPackages.cabal-install]; });
in
  env
