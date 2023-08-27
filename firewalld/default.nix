{ pkgs ? (import <nixpkgs> {})
, cracketSource ? pkgs.fetchFromGitHub
  {
    owner = "corpix";
    repo = "cracket";
    rev = "2d5511e0c734968a50bcda7d384bc203bed1f133";
    sha256 = "sha256-gqww8WYsUUcKPdctTt5hAlAZTJOqn0tMruD1wJEBkoM=";
  }
, racketPackages ? ({pkgs ? (import <nixpkgs> {})}:
  with pkgs;
  {
    "corpix-firewalld" = "${cracketSource}/firewalld";
    "corpix-net" = "${cracketSource}/net";
    "corpix-bnf" = "${cracketSource}/bnf";
    "corpix-time" = "${cracketSource}/time";
    "corpix-hex" = "${cracketSource}/hex";
    "corpix-prometheus" = "${cracketSource}/prometheus";
    "corpix-syntax" = "${cracketSource}/syntax";

    "racket-lib" = fetchzip {
      "hash" = "sha256-/BUFNHFmAtXAZAB9IkDecX+l8tf21AYhiNurpLGSiug=";
      "url" = "https://download.racket-lang.org/releases/8.8/pkgs/racket-lib.zip";
      "stripRoot" = false;
    };
    "rackunit-lib" = fetchzip {
      "hash" = "sha256-m/KKhkN60/peJ11Th7g+/BklztFN5+49r7R8u+WKT6g=";
      "url" = "https://download.racket-lang.org/releases/8.8/pkgs/rackunit-lib.zip";
      "stripRoot" = false;
    };
    "testing-util-lib" = fetchzip {
      "hash" = "sha256-x88VSdVAH2D3Py1vDj0Yllzbg9TdmYaYCqhiJDA2HjE=";
      "url" = "https://download.racket-lang.org/releases/8.8/pkgs/testing-util-lib.zip";
      "stripRoot" = false;
    };
    "base" = fetchzip {
      "hash" = "sha256-cRPRUS3wUt6D5mPYOIQWDSLursh14G1SACJVB6PYeuE=";
      "url" = "https://download.racket-lang.org/releases/8.8/pkgs/base.zip";
      "stripRoot" = false;
    };
  }) { inherit pkgs; }
}:
with pkgs;
with lib;
stdenv.mkDerivation rec {
  name = "corpix-firewalld";
  buildInputs = [
    racket
    findutils
    makeWrapper
  ];
  nativeBuildInputs = [makeWrapper];

  unpackPhase = ":";
  buildPhase = ''
    mkdir pkgs
    export HOME=$(pwd)

    ${(concatMapStringsSep "\n" (name: ''
      cp -r ${racketPackages.${name}} $HOME/pkgs/${name}
      find $HOME/pkgs/${name} -type d | xargs chmod 755
    '') (attrNames racketPackages))}
    racket -l- pkg/dirs-catalog --link $HOME/catalog $HOME/pkgs
    raco pkg config --set catalogs file://$HOME/catalog
    export PLTADDONDIR=$HOME/.local/share/racket
  '';
  installPhase = ''
    raco pkg install --binary --batch --auto --user ${name}
    mkdir -p $out/bin
    raco exe -o $out/bin/firewalld ${cracketSource}/firewalld/firewalld/firewalld.rkt
    wrapProgram $out/bin/firewalld --prefix PATH ":" ${makeBinPath [libcap iptables]}
  '';
}
