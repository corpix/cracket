{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let
  inherit (lib)
    concatMapStringsSep
  ;

  ##

  mkPackage = { name, src, collects }: stdenv.mkDerivation {
    inherit name src;
    buildPhase = ":";
    installPhase = ''
      mkdir -p $out/share/racket/collects
      ${concatMapStringsSep "\n"
        (collect:
          ''
            d=$(dirname "${collect.target}")
            if [ "$d" != "." ]
            then
               mkdir -p $out/share/racket/collects/"$d"
            fi
            cp -r ${collect.source} $out/share/racket/collects/${collect.target}
          '')
        collects}
    '';
  };
in rec {
  net-ip = mkPackage {
    name = "racket-net-ip";
    src = fetchFromGitHub {
      owner = "Bogdanp";
      repo = "racket-net-ip";
      rev = "b1cbedd097af93037427ca7052e7bbd32c531433";
      sha256 = "sha256-vXwZSNLTz8xYGX7XUXEo0zm+Iy1V7BRnI4Uabjy5DIY=";
    };
    collects = [
      { source = "net-ip-lib/net";
        target = "net";
      }
    ];
  };

  ##

  corpix-bnf = mkPackage {
    name = "racket-corpix-bnf";
    src = ./bnf;
    collects = [
      { source = "bnf.rkt";
        target = "corpix/bnf.rkt";
      }
    ];
  };
  corpix-bytes = mkPackage {
    name = "racket-corpix-bytes";
    src = ./bytes;
    collects = [
      { source = "bytes.rkt";
        target = "corpix/bytes.rkt";
      }
    ];
  };
  corpix-hex = mkPackage {
    name = "racket-corpix-hex";
    src = ./hex;
    collects = [
      { source = "hex.rkt";
        target = "corpix/hex.rkt"; }
    ];
  };
  corpix-http = mkPackage {
    name = "racket-corpix-http";
    src = ./http;
    collects = [
      { source = "http.rkt";
        target = "corpix/http.rkt"; }
    ];
  };
  corpix-json = mkPackage {
    name = "racket-corpix-json";
    src = ./json;
    collects = [
      { source = "json.rkt";
        target = "corpix/json.rkt"; }
    ];
  };
  corpix-multipart = mkPackage {
    name = "racket-corpix-multipart";
    src = ./multipart;
    collects = [
      { source = "multipart.rkt";
        target = "corpix/multipart.rkt"; }
    ];
  };
  corpix-prometheus = mkPackage {
    name = "racket-corpix-prometheus";
    src = ./prometheus;
    collects = [
      { source = "prometheus.rkt";
        target = "corpix/prometheus.rkt"; }
    ];
  };
  corpix-task = mkPackage {
    name = "racket-corpix-task";
    src = ./task;
    collects = [
      { source = "task.rkt";
        target = "corpix/task/task.rkt"; }
      { source = "syntax.rkt";
        target = "corpix/task/syntax.rkt"; }
      { source = "lang.rkt";
        target = "corpix/task/lang.rkt"; }
    ];
  };
  corpix-telegram = mkPackage {
    name = "racket-corpix-telegram";
    src = ./telegram;
    collects = [
      { source = "telegram.rkt";
        target = "corpix/telegram.rkt"; }
    ];
  };
  corpix-time = mkPackage {
    name = "racket-corpix-time";
    src = ./time;
    collects = [
      { source = "time.rkt";
        target = "corpix/time.rkt"; }
    ];
  };
  corpix-url = mkPackage {
    name = "racket-corpix-url";
    src = ./url;
    collects = [
      { source = "url.rkt";
        target = "corpix/url.rkt"; }
    ];
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
        corpix-bytes
        corpix-hex
        corpix-http
        corpix-json
        corpix-multipart
        corpix-prometheus
        corpix-task
        corpix-telegram
        corpix-time
        corpix-url
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
