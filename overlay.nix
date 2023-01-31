{ pkgs ? import <nixpkgs> {} }:
with pkgs;
rec {
  net-ip = stdenv.mkDerivation {
    name = "racket-net-ip";
    src = fetchFromGitHub {
      owner = "Bogdanp";
      repo = "racket-net-ip";
      rev = "b1cbedd097af93037427ca7052e7bbd32c531433";
      sha256 = "sha256-vXwZSNLTz8xYGX7XUXEo0zm+Iy1V7BRnI4Uabjy5DIY=";
    };
    buildPhase = ":";
    installPhase = ''
      mkdir -p $out/share/racket/collects/net
      cp -r net-ip-lib/net/* $out/share/racket/collects/net
    '';
  };
  corpix-bnf = stdenv.mkDerivation {
    name = "racket-corpix-bnf";
    src = ./bnf;
    buildPhase = ":";
    installPhase = ''
      mkdir -p $out/share/racket/collects/corpix
      cp bnf.rkt $out/share/racket/collects/corpix
    '';
  };
  corpix-time = stdenv.mkDerivation {
    name = "racket-corpix-time";
    src = ./time;
    buildPhase = ":";
    installPhase = ''
      mkdir -p $out/share/racket/collects/corpix
      cp time.rkt $out/share/racket/collects/corpix
    '';
  };
  corpix-hex = stdenv.mkDerivation {
    name = "racket-corpix-hex";
    src = ./hex;
    buildPhase = ":";
    installPhase = ''
      mkdir -p $out/share/racket/collects/corpix
      cp hex.rkt $out/share/racket/collects/corpix
    '';
  };
  corpix-prometheus = stdenv.mkDerivation {
    name = "racket-corpix-prometheus";
    src = ./prometheus;
    buildPhase = ":";
    installPhase = ''
      mkdir -p $out/share/racket/collects/corpix
      cp prometheus.rkt $out/share/racket/collects/corpix
    '';
  };

  ##

  cracket = stdenv.mkDerivation {
    name = "cracket";
    src = symlinkJoin {
      name = "cracket";
      paths = [
        racket
        net-ip
        corpix-bnf
        corpix-time
        corpix-hex
        corpix-prometheus
      ];
    };
    nativeBuildInputs = [makeWrapper];
    buildPhase = ":";
    installPhase = ''
      mkdir $out
      cp -ar $src/* $out/

      chmod 755 $out/bin
      wrapProgram $out/bin/raco --set PLTCOLLECTS $src/share/racket/collects
      wrapProgram $out/bin/racket --set PLTCOLLECTS $src/share/racket/collects
    '';
    fixupPhase = ":";
  };
}
