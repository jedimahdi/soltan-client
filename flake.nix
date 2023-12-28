{
  description = "Soltan Client";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = { nixpkgs, utils, easy-purescript-nix, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        easy-ps = import easy-purescript-nix { inherit pkgs; };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (_: _: { inherit (easy-ps) purs-tidy pulp spago purescript-language-server; purescript = easy-ps.purs-0_15_10; })
          ];
        };
      in
      {
        devShells.default = pkgs.mkShell
          {
            name = "soltan-client";
            buildInputs = with pkgs; [ gmp ];
            nativeBuildInputs = with pkgs; [ nodejs-18_x purs-tidy purescript-language-server spago purescript nodePackages.prettier gmp ];
            shellHook = ''
              export LD_LIBRARY_PATH="${
                pkgs.lib.makeLibraryPath
                (with pkgs; [ stdenv.cc.cc gmp zlib ncurses5 ])
              }:$LD_LIBRARY_PATH"
            '';
          };
      }
    );
}
