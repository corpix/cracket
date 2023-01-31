{ pkgs ? import <nixpkgs> {}, ... }:
with pkgs;
let
  overlay = import ../overlay.nix { inherit pkgs; };
in stdenv.mkDerivation {
  name = "firewalld";
  src = ./.;
  buildInputs = [overlay.cracket];
  buildPhase = ''
    raco exe -o firewalld firewalld.rkt
  '';
  installPhase = ''
    mkdir -p $out/bin
    mv firewalld $out/bin
  '';
}
