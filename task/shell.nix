let
  inherit (builtins)
    toString
  ;
  inherit (pkgs)
    writeScript
    stdenv
  ;
  inherit (pkgs.lib)
    concatStringsSep
    makeLibraryPath
  ;

  nixpkgs = <nixpkgs>;
  config = {};
  pkgs = import nixpkgs { inherit config; };

  shellWrapper = writeScript "shell-wrapper" ''
    #! ${stdenv.shell}
    set -e

    exec -a shell ${pkgs.fish}/bin/fish --login --interactive "$@"
  '';

  libs = with pkgs; [
    openssl
    racket
    libgit2
  ];
  libPath = makeLibraryPath libs;
in stdenv.mkDerivation rec {
  name = "nix-shell";
  buildInputs = with pkgs; [
    glibcLocales bashInteractive man
    nix cacert utillinux coreutils
    git findutils gnumake

    racket pkg-config
  ];
  shellHook = with pkgs; ''
    export root=$(pwd)

    export LANG="en_US.UTF-8"
    export NIX_PATH="nixpkgs=${nixpkgs}"

    export LD_LIBRARY_PATH="${libPath}"
    export CFLAGS="-I${racket}/include/racket"
    export LDFLAGS="-L${concatStringsSep
      " -L"
      (map (v: "${toString v}/lib") libs)}"

    if [ -z "$CI" ]
    then
      export SHELL="${shellWrapper}"
      exec "$SHELL"
    fi
  '';
}
