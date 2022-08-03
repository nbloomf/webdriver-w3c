{ nixpkgs ? (import (builtins.fetchTarball {
    name = "nixos-master";
    url = https://github.com/NixOS/nixpkgs/archive/c4a9eafb61919a075059075340bda90394d35f93.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1rc0m28wywzkniz9y871fdv7dhml4f4xdgcfmbv5s5bicb0pyma5";
  }) {}), compiler ? "ghc8107" }:
let
  pkgs = nixpkgs.pkgs;
  haskLib = pkgs.haskell.lib;
  overriddenHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      script-monad = haskLib.dontCheck (super.callHackage "script-monad" "0.0.4" {});
      webdriver-w3c = haskLib.dontCheck (super.callCabal2nix "webdriver-w3c" ./. {});
    };
  };
  webdriver-w3c = overriddenHaskellPackages.webdriver-w3c;
in
  webdriver-w3c 
