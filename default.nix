{ nixpkgs ? import <nixpkgs> {} }:
let
  contravariant-src = nixpkgs.fetchFromGitHub {
              owner = "ekmett";
              repo = "contravariant";
              rev = "5e39b8e89e426077a4e2ef5bd870b686af76cf94";
              sha256 = "0w3ggcfwh6r69a5c9mcq9alll1m0cawdr7yghzmkkkisvx65gdyp";
            };

  base-compat-src = nixpkgs.fetchFromGitHub {
              owner = "haskell-compat";
              repo = "base-compat";
              rev = "feda8f5f39d47c701eb9edc2efb07cc97bcf3542";
              sha256 = "1lv2s5bpmy7dfp51gl2xgjfc4a94if6a7rzcj5wzwc6fkkkl1iap";
            };

  base-compat-batteries = haskellPackages.callCabal2nix "base-compat-batteries" (base-compat-src + "/base-compat-batteries") {};
  base-compat = haskellPackages.callCabal2nix "base-compat" (base-compat-src + "/base-compat") {};
  contravariant = haskellPackages.callCabal2nix "contravariant" contravariant-src {};

  hledger-source = nixpkgs.fetchFromGitHub {
              owner = "simonmichael";
              repo = "hledger";
              rev = "22f2e90a4b78bc86b4993184738cfa0a077c3d43";
              sha256 = "1ljifqa04hw5b93jr1y4yslcsaq2wd31yb3vkihs90v1x599z0cn";
            };

  hledger-lib = haskellPackages.callCabal2nix "hledger-lib" (hledger-source + "/hledger-lib") {};

  # haskellPackages = nixpkgs.haskellPackages;
  haskellPackages = nixpkgs.haskellPackages.override {
    overrides= self: super: {
      # inherit base-compat-batteries hledger-lib base-compat contravariant;
      inherit hledger-lib;
    };
  };

in
  haskellPackages.callPackage ./invoice.nix {}
  # hledger-lib
