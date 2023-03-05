{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs)
    makeWrapper
  ;
  inherit (pkgs.lib)
    concatMapStringsSep
  ;

in {
  cracket = let
    inputs = [
      { path = ./syntax.rkt; name = "syntax.rkt"; }
      { path = ./net.rkt; name = "net.rkt"; }
      { path = ./bnf.rkt; name = "bnf.rkt"; }
      { path = ./bytes.rkt; name = "bytes.rkt"; }
      { path = ./hex.rkt; name = "hex.rkt"; }
      { path = ./http.rkt; name = "http.rkt"; }
      { path = ./json.rkt; name = "json.rkt"; }
      { path = ./multipart.rkt; name = "multipart.rkt"; }
      { path = ./prometheus.rkt; name = "prometheus.rkt"; }
      { path = ./task.rkt; name = "task.rkt"; }
      { path = ./task-lang.rkt; name = "task-lang.rkt"; }
      { path = ./telegram.rkt; name = "telegram.rkt"; }
      { path = ./time.rkt; name = "time.rkt"; }
      { path = ./url.rkt; name = "url.rkt"; }
      { path = ./quirc.rkt; name = "quirc.rkt"; }
    ];
  in pkgs.racket.overrideAttrs (self: super: {
    nativeBuildInputs = super.nativeBuildInputs ++ [makeWrapper];
    postPatch = ''
      mkdir -p collects/corpix
      ${concatMapStringsSep "\n"
        (input: "cp ${input.path} collects/corpix/${input.name}")
        inputs}
    '';
    postInstall = ''
      for executable in $out/bin/*
      do
        wrapProgram $executable --prefix LD_LIBRARY_PATH : ${pkgs.quirc.outPath}/lib
      done
    '';
  });
}
