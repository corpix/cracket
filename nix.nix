({pkgs ? (import <nixpkgs> {

}), racketPackages ? (({pkgs ? (import <nixpkgs> {

})}: with pkgs;
  {
    "corpix-url" = /home/user/projects/src/git.backbone/corpix/cracket/url;
    "corpix-http" = /home/user/projects/src/git.backbone/corpix/cracket/http;
    "corpix-multipart" = /home/user/projects/src/git.backbone/corpix/cracket/multipart;
    "corpix-bytes" = /home/user/projects/src/git.backbone/corpix/cracket/bytes;
    "rackunit-lib" = (fetchzip {
      "hash" = "sha256-aj6jnBFZOKX7SBUyZ2B24AyFTKiF6MZ0e6+vTm1Lxfo=";
      "url" = "https://download.racket-lang.org/releases/8.7/pkgs/rackunit-lib.zip";
      "stripRoot" = false;
    });
    "testing-util-lib" = (fetchzip {
      "hash" = "sha256-9mPXZbxegNAPhSRuYH2A4+ZM6DXKSNjIRDt9XXOaznM=";
      "url" = "https://download.racket-lang.org/releases/8.7/pkgs/testing-util-lib.zip";
      "stripRoot" = false;
    });
    "base" = (fetchzip {
      "hash" = "sha256-qUgPw2fDSxk2LBUnEJRH1VQgKRqB3YE9/85Hlke9lp8=";
      "url" = "https://download.racket-lang.org/releases/8.7/pkgs/base.zip";
      "stripRoot" = false;
    });
    "racket-lib" = (fetchzip {
      "hash" = "sha256-QysWBjmr2XrQIfJPwrR+o0tmufRiTpxIByeZ7jAsvzQ=";
      "url" = "https://download.racket-lang.org/releases/8.7/pkgs/racket-lib.zip";
      "stripRoot" = false;
    });
    "corpix-hex" = /home/user/projects/src/git.backbone/corpix/cracket/hex;
  }) {
  "pkgs" = pkgs;
})}: with pkgs;
  with lib;
    (stdenv.mkDerivation rec {
      "buildInputs" = [
        racket
        findutils
        makeWrapper
      ];
      "name" = "corpix-http";
      "installPhase" = ''
        raco pkg install --binary --batch --auto --user ${name}
        mkdir $out/bin
        ln -s ${racket}/bin/racket $out/bin/racket
        wrapProgram $out/bin/racket --set PLTADDONDIR $out/.local/share/racket
      '';
      "buildPhase" = ''
        mkdir $out
        export HOME=$out
        mkdir $out/pkgs
        ${(concatMapStringsSep "\n" (name: ''
          cp -r ${racketPackages.${name}} $out/pkgs/${name}
          find $out/pkgs/${name} -type d | xargs chmod 755
        '') (attrNames racketPackages))}
        racket -l- pkg/dirs-catalog --link $out/catalog $out/pkgs
        raco pkg config --set catalogs file://$out/catalog
      '';
      "unpackPhase" = ":";
    }))
