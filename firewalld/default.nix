{ pkgs ? import <nixpkgs> {}, ... }:
let
  inherit (pkgs)
    fetchFromGitHub
    stdenv
    makeWrapper
    cracket
    libcap
    iptables
  ;
  inherit (pkgs.lib)
    makeBinPath
  ;

  overlay = import ../overlay.nix { inherit pkgs; };
in stdenv.mkDerivation {
  name = "firewalld";
  src = ./.;
  buildInputs = [overlay.cracket];
  nativeBuildInputs = [makeWrapper];
  buildPhase = ''
    raco exe -o firewalld firewalld.rkt
  '';
  installPhase = ''
    mkdir -p $out/bin
    mv firewalld $out/bin
    wrapProgram $out/bin/firewalld --prefix PATH ":" ${makeBinPath [libcap iptables]} 
  '';
}
