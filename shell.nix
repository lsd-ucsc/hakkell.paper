{ config ? { }
, pkgs ? import
    (
      fetchTarball {
        # latest https://github.com/NixOS/nixpkgs/commits/nixos-22.11 as of Tue 16 May 2023 01:36:07 PM PDT
        url = https://github.com/NixOS/nixpkgs/archive/55af203d468a6f5032a519cba4f41acf5a74b638.tar.gz;
        # obtained by nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/$REV.tar.gz
        sha256 = "1174h8wbvm03qgbz8mw0iazh2zgl08rh2d59b26dqlc0an6idi84";
      }
    )
    { inherit config; }
}:

with pkgs;

mkShell {
  name = "hakkell-environment";
  buildInputs = [
    texlive.combined.scheme-full
    (haskellPackages.ghcWithPackages (p: with p; [
      random
      criterion
      lhs2tex
      threadscope
    ]))
    ghcid
    python3 # for the noprint.py script
    gnuplot
    inkscape
    zip
    unzip
  ];
  passthru = {
    inherit pkgs;
  };
}
