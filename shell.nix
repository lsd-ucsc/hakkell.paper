# nix-shell \
#   -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/7e14b746c01a1a8123c9d78fe01e638872a9720f.tar.gz \
#   -p texlive.combined.scheme-full

{ config ? {}
, pkgs ? import (
    fetchTarball {
      # latest https://github.com/NixOS/nixpkgs/commits/nixos-22.11 as of Tue 16 May 2023 01:36:07 PM PDT
      url = https://github.com/NixOS/nixpkgs/archive/55af203d468a6f5032a519cba4f41acf5a74b638.tar.gz;
      # obtained by nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/$REV.tar.gz
      sha256 = "1174h8wbvm03qgbz8mw0iazh2zgl08rh2d59b26dqlc0an6idi84";
    }
  ) { inherit config; }
}:

with pkgs;

mkShell {
  name = "tex-environment";
  buildInputs = [
    texlive.combined.scheme-full
    (haskellPackages.ghcWithPackages (p: with p; [random async criterion doctest QuickCheck lhs2tex]))
    ghcid
  ];
}
