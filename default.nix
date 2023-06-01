let
  shenv = import ./shell.nix { };
  pkgs = shenv.passthru.pkgs;
in
pkgs.stdenv.mkDerivation {
  name = "hakkell";
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  buildInputs = shenv.buildInputs;
  buildPhase = ''
    make main.pdf
    make main.bench.elf CAPABILITIES= # allow variable capabilities
  '';
  installPhase = ''
    mkdir -pv $out/bin
    mv -v main.pdf $out/hakkell-paper.pdf
    mv -v main.bench.elf $out/bin/hakkell-bench.elf
  '';
}
