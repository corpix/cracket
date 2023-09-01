{ pkgs ? (import <nixpkgs> {})
, cracketSource ? ./..
  # pkgs.fetchFromGitHub
  # {
  #   owner = "corpix";
  #   repo = "cracket";
  #   rev = "c13bcf0238d6cdad9099e7ccdd0c05a47d737aba";
  #   sha256 = "sha256-/jeS2DqP+Pz4QsNzWkuXEk3CH70yYsapVcrTm3nqefU=";
  # }
, racketPackages ? ({pkgs ? (import <nixpkgs> {})}:
  with pkgs;
  {
    "cext-lib" = (fetchzip {
      "hash" = "sha256-wRbQOIqXSwZia5I4rI+1MIYeMmC2YCUmCQQhfVG2wgU=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/cext-lib.zip";
      "stripRoot" = false;
    });
    "images-gui-lib" = (fetchzip {
      "hash" = "sha256-TJB/Av1M8MUnF3AgByVS/5lIz4b3rV+h7gd4nTTemTY=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/images-gui-lib.zip";
      "stripRoot" = false;
    });
    "gui-doc" = (fetchzip {
      "hash" = "sha256-qqrJyfOa+zXlRrWV3pAE20tKFUhnMn4zFkfK39U3RmE=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/gui-doc.zip";
      "stripRoot" = false;
    });
    "dynext-lib" = (fetchzip {
      "hash" = "sha256-4mTtsPgpNJfL7dYo0FmbDpDFhqrQ5n3qpL0OAcFuBzk=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/dynext-lib.zip";
      "stripRoot" = false;
    });
    "compatibility-doc" = (fetchzip {
      "hash" = "sha256-NYF4pJgRRmeuop3/f0tTtHPazkrC5uQxynBBL0zDj+0=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/compatibility-doc.zip";
      "stripRoot" = false;
    });
    "cldr-core" = (fetchgit {
      "hash" = "sha256-Tpk6uYWz4//C+/n50wsLiD16rwOim85R/Ykrtcoa1+8=";
      "rev" = "c9b80777c422c3b104bb85052d74a2dc1535a3c3";
      "url" = "https://github.com/97jaz/cldr-core.git";
    });
    "snip-lib" = (fetchzip {
      "hash" = "sha256-s59oMQhrNeRrg0QOWw5wZFX8BAOKRS0qjKtXptgsdsI=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/snip-lib.zip";
      "stripRoot" = false;
    });
    "net-cookies-doc" = (fetchzip {
      "hash" = "sha256-1cnu4m5TnB4qI2N43ac2rvDVmp5tdneYvsQDxNqc2Jg=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/net-cookies-doc.zip";
      "stripRoot" = false;
    });
    "plot-gui-lib" = (fetchzip {
      "hash" = "sha256-4sw91/xQpYhF3XDwLwhrY+7Ior2FfynIH7xW1yiPbVw=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/plot-gui-lib.zip";
      "stripRoot" = false;
    });
    "typed-racket-compatibility" = (fetchzip {
      "hash" = "sha256-WyKYWYeI2WnAV7IWUsaPCV++r3Hka+xKYArGYfGulpI=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/typed-racket-compatibility.zip";
      "stripRoot" = false;
    });
    "unstable-pretty-lib" = (fetchgit {
      "hash" = "sha256-aYGw/oj95ttqHwKKFLbpOBa2e6eWCohOTH8l1cxx33c=";
      "rev" = "d420f822301174b1931c8b43d2131924fc75565f";
      "url" = "https://github.com/racket/unstable-pretty-lib.git";
    });
    "images-lib" = (fetchzip {
      "hash" = "sha256-mk1Cibi3Ib/kbLgDcn7iCGUZ9vVEmzaQbYy3vql3RI8=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/images-lib.zip";
      "stripRoot" = false;
    });
    "string-constants-lib" = (fetchzip {
      "hash" = "sha256-xmschNGn+gh48CEaFSSJjebFrdcvQTxun8yALkMw4l4=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/string-constants-lib.zip";
      "stripRoot" = false;
    });
    "tex-table" = (fetchzip {
      "hash" = "sha256-gIGWxnG/f0mlXklZqSWE5mPXAChsbmUl+o/KxoMg8qY=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/tex-table.zip";
      "stripRoot" = false;
    });
    "draw-doc" = (fetchzip {
      "hash" = "sha256-bfTirLboT8Zu9XWmZ1YOxOCaTMuvN50zhh9GG3UbTcs=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/draw-doc.zip";
      "stripRoot" = false;
    });
    "rackunit-gui" = (fetchzip {
      "hash" = "sha256-2L6KVzTIYPqfMrNc+aLFx6ZsR8ky75cmkYh7RbQrwCg=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/rackunit-gui.zip";
      "stripRoot" = false;
    });
    "draw-lib" = (fetchzip {
      "hash" = "sha256-PcuwFtTJyIB33D4CQt1YpM+H13vuqKreLX/tbydYq9A=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/draw-lib.zip";
      "stripRoot" = false;
    });
    "net-cookies-lib" = (fetchzip {
      "hash" = "sha256-uKn3cwT/qFJ1lW/TmEOla9ezAVSEkCDK9k8+kFp741I=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/net-cookies-lib.zip";
      "stripRoot" = false;
    });
    "plot-compat" = (fetchzip {
      "hash" = "sha256-dhYMPPUOyQD/hZBqUDCMqn0UXHTyIGP83rt2jZGk2QU=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/plot-compat.zip";
      "stripRoot" = false;
    });
    "gregor" = (builtins.toPath "${(fetchgit {
      "hash" = "sha256-Ljnoi9x/j3VJpBtfvCBYL4gnlhBpq+o1o7/vioC2T10=";
      "rev" = "2d20192e8795e01a1671869dddaf1984f0cbafee";
      "url" = "https://github.com/97jaz/gregor.git";
    })}/gregor");
    "db-doc" = (fetchzip {
      "hash" = "sha256-D8dBdViCXpF/K1wCPEl9aOiRNpsPImOjfmSuVfRzhpE=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/db-doc.zip";
      "stripRoot" = false;
    });
    "planet-lib" = (fetchzip {
      "hash" = "sha256-Hq8wRj850Pu5J920wYwqdOCsct/+7EeWAdUojGHJnX8=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/planet-lib.zip";
      "stripRoot" = false;
    });
    "cldr-localenames-modern" = (fetchgit {
      "hash" = "sha256-fZ1fnkslpZuicJgMh6/aLd4rPov7lvJr6ulDWpTMpKg=";
      "rev" = "f9f3e8d9245764a309542816acf40fe147b473a3";
      "url" = "https://github.com/97jaz/cldr-localenames-modern.git";
    });
    "typed-racket-more" = (fetchzip {
      "hash" = "sha256-szVeMQf781mpKTw+ntoDDUbnvv4dRvIdsuxk0kU1ti8=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/typed-racket-more.zip";
      "stripRoot" = false;
    });
    "syntax-color-doc" = (fetchzip {
      "hash" = "sha256-POonVOlYPK5ir3dJdJ+dHTfkprCU7HTxpP40+5/g8A8=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/syntax-color-doc.zip";
      "stripRoot" = false;
    });
    "pict-lib" = (fetchzip {
      "hash" = "sha256-LuW7eLl7fM93O+x+fvFhHo00VTTxmAG4Y6/NJmAmuoE=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/pict-lib.zip";
      "stripRoot" = false;
    });
    "scribble-lib" = (fetchzip {
      "hash" = "sha256-M23MZTh9xPazxSKY4VF1QoIDyd1NfqZvY5pxwM16bCI=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/scribble-lib.zip";
      "stripRoot" = false;
    });
    "readline-doc" = (fetchzip {
      "hash" = "sha256-NX8tyYaNXEwYxH7uQoRdvK9LyVNHMKB4d6PY66SvocQ=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/readline-doc.zip";
      "stripRoot" = false;
    });
    "nanopass" = (fetchgit {
      "hash" = "sha256-2VftyKsZhWPIeLq5vpSI2NWS2mryivSLEgknZ5+g3n0=";
      "rev" = "deac3a4bf937e1217ec54c5439710712b227fc5a";
      "url" = "https://github.com/nanopass/nanopass-framework-racket.git";
    });
    "sandbox-lib" = (fetchzip {
      "hash" = "sha256-OZO0rSfB+lL1IMQTYuMqsPkAFBRN07Gf/v6y1jUnsws=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/sandbox-lib.zip";
      "stripRoot" = false;
    });
    "srfi-doc" = (fetchzip {
      "hash" = "sha256-76ZeEPXqEVbMNQI+08bkMsLX/3aUZpte1nwxyboqr4c=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/srfi-doc.zip";
      "stripRoot" = false;
    });
    "simple-tree-text-markup-doc" = (fetchzip {
      "hash" = "sha256-TYfygrlog+slmIjxP2+Cq7OWpsn7FYzUz0HUBitPpB8=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/simple-tree-text-markup-doc.zip";
      "stripRoot" = false;
    });
    "racket-index" = (fetchzip {
      "hash" = "sha256-o3rok43FAKVzpYqmAZznkGVRgwWHonRUn6k8imVN/gA=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/racket-index.zip";
      "stripRoot" = false;
    });
    "data-enumerate-lib" = (fetchzip {
      "hash" = "sha256-ZTUHVLKnuRbkvNI7v3S4eilwYEgc/7+87HFePu98MW8=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/data-enumerate-lib.zip";
      "stripRoot" = false;
    });
    "readline-lib" = (fetchzip {
      "hash" = "sha256-dyheap1O8ZElS2iHnQLOin4lRpp8KyJ3z2SbLZNKZU0=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/readline-lib.zip";
      "stripRoot" = false;
    });
    "scheme-lib" = (fetchzip {
      "hash" = "sha256-sNUICpmz5O13NKuHYNBqE7Ai4Z4T6c+HESai+ip17KY=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/scheme-lib.zip";
      "stripRoot" = false;
    });
    "option-contract-lib" = (fetchzip {
      "hash" = "sha256-Mw/t4r2cNx+UEM/a+cEQLuhVRk3rDHJg+C8fbx2GRGk=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/option-contract-lib.zip";
      "stripRoot" = false;
    });
    "slideshow-lib" = (fetchzip {
      "hash" = "sha256-Z8QdKxq7pN23p52PnMqTH83CL2ziNrfrsh3ITgq6bWE=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/slideshow-lib.zip";
      "stripRoot" = false;
    });
    "future-visualizer" = (fetchzip {
      "hash" = "sha256-G7OdbPdoI52+WoMHlaBloBlwd0e7pabBEMTXpPRUblw=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/future-visualizer.zip";
      "stripRoot" = false;
    });
    "icons" = (fetchzip {
      "hash" = "sha256-e9CYxMhgw8yZ65Aczzx4mpDU6vqdKoBzg6rJTkznfbU=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/icons.zip";
      "stripRoot" = false;
    });
    "distributed-places-doc" = (fetchzip {
      "hash" = "sha256-XX8+m523bILBjGWebo6ynLny24C85aCEG/PVkn0fnlQ=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/distributed-places-doc.zip";
      "stripRoot" = false;
    });
    "parser-tools-lib" = (fetchzip {
      "hash" = "sha256-C0XEks3hTUF1uMyfUaoq5TVSzLUIv13wUt/nXqN1yPs=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/parser-tools-lib.zip";
      "stripRoot" = false;
    });
    "math-lib" = (fetchzip {
      "hash" = "sha256-veuohdDeMwnP6LB6Y24ATjBxmpeuo1hwH5oERfu8fpM=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/math-lib.zip";
      "stripRoot" = false;
    });
    "r5rs-lib" = (fetchzip {
      "hash" = "sha256-1lgZCzGAwXBhr3eYqT99WANYdpuKMu85uJFDRJEjCtQ=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/r5rs-lib.zip";
      "stripRoot" = false;
    });
    "simple-tree-text-markup-lib" = (fetchzip {
      "hash" = "sha256-Q6QSbTiivMXhGak01g3MIkzjuzB8jjHubZsldRhHmUo=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/simple-tree-text-markup-lib.zip";
      "stripRoot" = false;
    });
    "distributed-places-lib" = (fetchzip {
      "hash" = "sha256-RIS+Ft5C/HjFnbI+MioiCf1GIn7EkXV/M+TQaW+ZPH4=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/distributed-places-lib.zip";
      "stripRoot" = false;
    });
    "srfi-lite-lib" = (fetchzip {
      "hash" = "sha256-cRlWMRme8Vkqu5d3pSo/AvP5qM4rdJb7t+5Co5d8fyM=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/srfi-lite-lib.zip";
      "stripRoot" = false;
    });
    "rackunit-lib" = (fetchzip {
      "hash" = "sha256-XoCRCoHKPT0vmZtgRR9IL3dYTBeFpcW2hG0bzGg/3IU=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/rackunit-lib.zip";
      "stripRoot" = false;
    });
    "base" = (fetchzip {
      "hash" = "sha256-WClQXAhal69cakC17PhkyxK88ZDgPcBeTqbI/hfQKr8=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/base.zip";
      "stripRoot" = false;
    });
    "net-doc" = (fetchzip {
      "hash" = "sha256-1Rcyi9wYiL2beDe0rNKA8mhThfRqj7H031V4vn8LcKA=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/net-doc.zip";
      "stripRoot" = false;
    });
    "web-server-doc" = (fetchzip {
      "hash" = "sha256-cWDHdN4JtHh0vv+sS0OlgjdhhhPAzD1XppenOW05uug=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/web-server-doc.zip";
      "stripRoot" = false;
    });
    "gregor-lib" = (builtins.toPath "${(fetchgit {
      "hash" = "sha256-Ljnoi9x/j3VJpBtfvCBYL4gnlhBpq+o1o7/vioC2T10=";
      "rev" = "2d20192e8795e01a1671869dddaf1984f0cbafee";
      "url" = "https://github.com/97jaz/gregor.git";
    })}/gregor-lib");
    "rackunit-doc" = (fetchzip {
      "hash" = "sha256-laZjNaUpZxz9ahoCkfeD6Ux36HDHIIN06V6h7IqU1kY=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/rackunit-doc.zip";
      "stripRoot" = false;
    });
    "testing-util-lib" = (fetchzip {
      "hash" = "sha256-jpUCTJNaQxtiUv4BqCIxCadTUrUKhOI9835YD8DmKis=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/testing-util-lib.zip";
      "stripRoot" = false;
    });
    "compiler-lib" = (fetchzip {
      "hash" = "sha256-y1dV25C12JgL5iQe4aK0wrBQxzq8HlptToK0soTcYno=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/compiler-lib.zip";
      "stripRoot" = false;
    });
    "at-exp-lib" = (fetchzip {
      "hash" = "sha256-fDs3npzA4x/Dz7E9VCrdkgt5mQEgxdbbkI4TyBWfmtk=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/at-exp-lib.zip";
      "stripRoot" = false;
    });
    "unix-socket-lib" = (fetchzip {
      "hash" = "sha256-FpuStKMC5nOkCPmkSx6PdjuZUmqmCBnO8gJMlLAQqkc=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/unix-socket-lib.zip";
      "stripRoot" = false;
    });
    "math-doc" = (fetchzip {
      "hash" = "sha256-Qygsiu0IqNysf4A2Ao0ot1Gzh9zd48FvQjXa183NJuA=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/math-doc.zip";
      "stripRoot" = false;
    });
    "threading-lib" = (builtins.toPath "${(fetchgit {
      "hash" = "sha256-KH60J/MQawwNBTOpdeD12EFbgiVV4BZavdUoVaFOQdE=";
      "rev" = "1f3b630d3ed78edc6d883f9d99305158d6b67623";
      "url" = "https://github.com/lexi-lambda/threading.git";
    })}/threading-lib");
    "syntax-color-lib" = (fetchzip {
      "hash" = "sha256-YtO0del58VBplCBIvQjiGxzWN/WKvkOkL3yUj2VFdJY=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/syntax-color-lib.zip";
      "stripRoot" = false;
    });
    "macro-debugger-text-lib" = (fetchzip {
      "hash" = "sha256-7UXobe870jKYLtoiX1FRWUuXDct7eL1EXF6APPdHd/Y=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/macro-debugger-text-lib.zip";
      "stripRoot" = false;
    });
    "web-server-lib" = (fetchzip {
      "hash" = "sha256-5J0C6tjK+kJjS+J5KftBg8d0sDSplvU1ttTTn3/NAbM=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/web-server-lib.zip";
      "stripRoot" = false;
    });
    "pconvert-lib" = (fetchzip {
      "hash" = "sha256-BybhtTAuiKotIcjr3HwdrqnXinJ1/x4UCt3Y742PVZY=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/pconvert-lib.zip";
      "stripRoot" = false;
    });
    "errortrace-doc" = (fetchzip {
      "hash" = "sha256-uds3M4Rr1prcufrPh/VU/9Bn2P2lNSwydVHM0tKe8hE=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/errortrace-doc.zip";
      "stripRoot" = false;
    });
    "future-visualizer-pict" = (fetchzip {
      "hash" = "sha256-Qk8q4wkkBujN0ov4CdQrxhORT9SMIR8rEZl45028xdw=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/future-visualizer-pict.zip";
      "stripRoot" = false;
    });
    "string-constants-doc" = (fetchzip {
      "hash" = "sha256-n/DWUQvlN75ntH7NFxreR6eo6HAWav/5tRL84e4gxeM=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/string-constants-doc.zip";
      "stripRoot" = false;
    });
    "cldr-numbers-modern" = (fetchgit {
      "hash" = "sha256-RDa1d4sSyfyuNgz2dJdu2f1XGiO4cPOkaseZ7q2cLJU=";
      "rev" = "625428099b3f8cd264955a283dddc176a6080ba1";
      "url" = "https://github.com/97jaz/cldr-numbers-modern.git";
    });
    "xrepl-doc" = (fetchzip {
      "hash" = "sha256-pyjulGImmRX5Wxi5tOAB8otv7Zt/a/Jr0dmpFYUG4OM=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/xrepl-doc.zip";
      "stripRoot" = false;
    });
    "profile-doc" = (fetchzip {
      "hash" = "sha256-uTfRusCM6QTqU0XNtwhLnv10zvSGu2h3x4uWEIdTvd8=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/profile-doc.zip";
      "stripRoot" = false;
    });
    "threading-doc" = (builtins.toPath "${(fetchgit {
      "hash" = "sha256-KH60J/MQawwNBTOpdeD12EFbgiVV4BZavdUoVaFOQdE=";
      "rev" = "1f3b630d3ed78edc6d883f9d99305158d6b67623";
      "url" = "https://github.com/lexi-lambda/threading.git";
    })}/threading-doc");
    "rackunit-typed" = (fetchzip {
      "hash" = "sha256-8OrBWneIR6uE78t37KuSPymVppLCJvX2bxCEKDiJFhE=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/rackunit-typed.zip";
      "stripRoot" = false;
    });
    "scribble-doc" = (fetchzip {
      "hash" = "sha256-9JflQoJUJY9xPJ5y1bMnDHxEAb1G/GliL/GQMN1DvmA=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/scribble-doc.zip";
      "stripRoot" = false;
    });
    "scribble-html-lib" = (fetchzip {
      "hash" = "sha256-gbuQvUD8hbogJdjpzKnyvqwDEFsxe5V1UTO5tt1REYI=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/scribble-html-lib.zip";
      "stripRoot" = false;
    });
    "zo-lib" = (fetchzip {
      "hash" = "sha256-luSqECvgf//7Id72+0yoIsIrbCfX7IdUkbrbFqwFtw8=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/zo-lib.zip";
      "stripRoot" = false;
    });
    "2d-lib" = (fetchzip {
      "hash" = "sha256-3KKnSIDbOD9+HQV3jEgav8+TuHdKM6dPIVWQPQXShgU=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/2d-lib.zip";
      "stripRoot" = false;
    });
    "mzscheme-doc" = (fetchzip {
      "hash" = "sha256-6Ys14c+Fsg+dWKx7DqgEfe9uTawbGmNPkoYzUF/rr6c=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/mzscheme-doc.zip";
      "stripRoot" = false;
    });
    "plot-doc" = (fetchzip {
      "hash" = "sha256-80PNzF5TgoZ5ymsbmZSC9riG70Ro4wmdRqDrK5kmA5c=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/plot-doc.zip";
      "stripRoot" = false;
    });
    "net-lib" = (fetchzip {
      "hash" = "sha256-AXLAhdG5AkcaKl03qXm+360RdOE6Eb+TZz8cGymidxk=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/net-lib.zip";
      "stripRoot" = false;
    });
    "r5rs-doc" = (fetchzip {
      "hash" = "sha256-cUdRGehtSLLdNp6RrgYOcfTv7v7WVeiCd2pAdTI3glQ=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/r5rs-doc.zip";
      "stripRoot" = false;
    });
    "gregor-doc" = (builtins.toPath "${(fetchgit {
      "hash" = "sha256-Ljnoi9x/j3VJpBtfvCBYL4gnlhBpq+o1o7/vioC2T10=";
      "rev" = "2d20192e8795e01a1671869dddaf1984f0cbafee";
      "url" = "https://github.com/97jaz/gregor.git";
    })}/gregor-doc");
    "cldr-dates-modern" = (fetchgit {
      "hash" = "sha256-byD2ubs543P9512lKD1JKB1ppyzjKzoWnuW8JPspa7M=";
      "rev" = "c36282917247f6a069e553535f4619007cd7b6e5";
      "url" = "https://github.com/97jaz/cldr-dates-modern.git";
    });
    "errortrace-lib" = (fetchzip {
      "hash" = "sha256-53ULV/vq2Uf3/T+aNv5KaUtY19T/XJzK8WWA6Eoipbg=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/errortrace-lib.zip";
      "stripRoot" = false;
    });
    "cldr-bcp47" = (fetchgit {
      "hash" = "sha256-YY5q44IQ1cNX4wk8Yt7B+z2uvfy+xMSl5tTDs+1RBlA=";
      "rev" = "823fc1a530f1a0ec4de59f5454c1a17f20c5a5d6";
      "url" = "https://github.com/97jaz/cldr-bcp47.git";
    });
    "typed-racket-doc" = (fetchzip {
      "hash" = "sha256-pbYCs+9LNxe1q9Bvqyr0iLfiP8qfqC6cv2rijX1b/dE=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/typed-racket-doc.zip";
      "stripRoot" = false;
    });
    "tzinfo" = (fetchgit {
      "hash" = "sha256-vvb3EZHFysa/2OiTat+i8zuALxiCPHNNaWCGlyPF6gk=";
      "rev" = "2f812283d9c90040aecb3c7e2ed2edf93a3720de";
      "url" = "https://github.com/97jaz/tzinfo.git";
    });
    "pict-doc" = (fetchzip {
      "hash" = "sha256-I0GLLjAd6OX16+GTlH1qlDsjehCg9phXVXUqjy+Q0ZA=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/pict-doc.zip";
      "stripRoot" = false;
    });
    "db-lib" = (fetchzip {
      "hash" = "sha256-h2/0Qbp3IZy5itB5Y9AvJ6+ZAECLalxAA+X9lKJ+T2Y=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/db-lib.zip";
      "stripRoot" = false;
    });
    "profile-lib" = (fetchzip {
      "hash" = "sha256-sPbhMTyPi8X6qqzjbgKR1wxkZxoZjuLk+8oh3dKC4vc=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/profile-lib.zip";
      "stripRoot" = false;
    });
    "compatibility-lib" = (fetchzip {
      "hash" = "sha256-GQSGxfc7SgqsMK+bgehgmpJ9NEicFF3q4ZtP3VXeyCk=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/compatibility-lib.zip";
      "stripRoot" = false;
    });
    "serialize-cstruct-lib" = (fetchzip {
      "hash" = "sha256-FfcPab/vkeOtzguJBsroRjtaktPY8qpfvvwFRpRI2sw=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/serialize-cstruct-lib.zip";
      "stripRoot" = false;
    });
    "gui-lib" = (fetchzip {
      "hash" = "sha256-K7bdP+8d4M5htno8ZZmj4FMkp8TwSk8d9dT8iFiQrwA=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/gui-lib.zip";
      "stripRoot" = false;
    });
    "slideshow-doc" = (fetchzip {
      "hash" = "sha256-WSO+cRwMXpyjJiUl14yqBfbNy66qi6pXD4zSUZTFv40=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/slideshow-doc.zip";
      "stripRoot" = false;
    });
    "r6rs-lib" = (fetchzip {
      "hash" = "sha256-ibTW5vjV5v1goLaoBVYCtrKxNzjOG0J8MnIzVh7FiOQ=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/r6rs-lib.zip";
      "stripRoot" = false;
    });
    "scheme-doc" = (fetchzip {
      "hash" = "sha256-FMScodFoJUjO185cBepkH2Klkty39YGCIYinDTfqHkA=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/scheme-doc.zip";
      "stripRoot" = false;
    });
    "r6rs-doc" = (fetchzip {
      "hash" = "sha256-SSVn88HYo2tCF6duhr2tZHlOUkDMW9p/fDAk0eH6p00=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/r6rs-doc.zip";
      "stripRoot" = false;
    });
    "threading" = (builtins.toPath "${(fetchgit {
      "hash" = "sha256-KH60J/MQawwNBTOpdeD12EFbgiVV4BZavdUoVaFOQdE=";
      "rev" = "1f3b630d3ed78edc6d883f9d99305158d6b67623";
      "url" = "https://github.com/lexi-lambda/threading.git";
    })}/threading");
    "data-doc" = (fetchzip {
      "hash" = "sha256-aHhNd4g/QQDZSiuxBfLq1wUhmwxtrXHY67jxg8qhjDg=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/data-doc.zip";
      "stripRoot" = false;
    });
    "data-lib" = (fetchzip {
      "hash" = "sha256-Ja4QZWP/a3h9Ll905QWAIfspUCozqMNSjF9GrK0Sr8w=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/data-lib.zip";
      "stripRoot" = false;
    });
    "db" = (fetchzip {
      "hash" = "sha256-2iYxMcC+q7iyqb4f5yGQz6XK0rVfLZM7j7MPDjiqQkk=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/db.zip";
      "stripRoot" = false;
    });
    "sasl-lib" = (fetchzip {
      "hash" = "sha256-xxf6mmoXE0O3LkWI+x9kGyTGQQ5Mo8oV6JcJbwAzl1I=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/sasl-lib.zip";
      "stripRoot" = false;
    });
    "plot-lib" = (fetchzip {
      "hash" = "sha256-v1X108dgZi2WbI3Kdy35RdsyLrMDGaNie5WuH9Q6saY=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/plot-lib.zip";
      "stripRoot" = false;
    });
    "planet-doc" = (fetchzip {
      "hash" = "sha256-Y1el3dmkZfPIT2HDmdOSLr9Z3qGlETlo/7vBNOJMK5I=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/planet-doc.zip";
      "stripRoot" = false;
    });
    "macro-debugger" = (fetchzip {
      "hash" = "sha256-L2o/l1ImRr5t4okgGzUV6lhUN3gcTjd8cILP6/QAg6Q=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/macro-debugger.zip";
      "stripRoot" = false;
    });
    "typed-racket-lib" = (fetchzip {
      "hash" = "sha256-fd84FWmc08FkhBgr5+FPYrC15IG9N9Urqak7dDXZxfU=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/typed-racket-lib.zip";
      "stripRoot" = false;
    });
    "source-syntax" = (fetchzip {
      "hash" = "sha256-UleVzLVgLyVmSCJTypDNxBwznRDNCR1o17FSr9b0fFg=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/source-syntax.zip";
      "stripRoot" = false;
    });
    "wxme-lib" = (fetchzip {
      "hash" = "sha256-xUYzJmmt1TZtxwpN6ilGQDNPthMzjj9d0ozSma/p1BM=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/wxme-lib.zip";
      "stripRoot" = false;
    });
    "xrepl-lib" = (fetchzip {
      "hash" = "sha256-+xP0+7lTX87Pvo7dY5cyAPpigwZI8DsAtkEQUSUW2Wk=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/xrepl-lib.zip";
      "stripRoot" = false;
    });
    "racket-lib" = (fetchzip {
      "hash" = "sha256-x3L08zudSaosm/oTwMfjfv0gkHQvNL7mTwfLlz1oHOI=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/racket-lib.zip";
      "stripRoot" = false;
    });
    "memoize-lib" = (builtins.toPath "${(fetchgit {
      "hash" = "sha256-Eun9HfbHXe5FfufsHSkoGZDzzRDQ8ftm0vGuL1yFB60=";
      "rev" = "911a0d3abe44fca1203425f6ff5767a9796f0c1f";
      "url" = "https://github.com/jbclements/memoize.git";
    })}/memoize-lib");
    "mime-type" = (builtins.toPath "${(fetchgit {
      "hash" = "sha256-zu+1Ds98Q+dLJtWeztK8MAhn+eabiyqUuHJL6nqjWNQ=";
      "rev" = "961742e5517ddf0cde130f0a18ca532a9734e16e";
      "url" = "https://github.com/Bogdanp/racket-net-mime-type.git";
    })}/mime-type");
    "mime-type-lib" = (builtins.toPath "${(fetchgit {
      "hash" = "sha256-zu+1Ds98Q+dLJtWeztK8MAhn+eabiyqUuHJL6nqjWNQ=";
      "rev" = "961742e5517ddf0cde130f0a18ca532a9734e16e";
      "url" = "https://github.com/Bogdanp/racket-net-mime-type.git";
    })}/mime-type-lib");
    "srfi-lib" = (fetchzip {
      "hash" = "sha256-xacU1y5oiLaP6w9K79MzgZR5RJ8XhNjs2W+WcIYwHaQ=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/srfi-lib.zip";
      "stripRoot" = false;
    });
    "racket-doc" = (fetchzip {
      "hash" = "sha256-i1yKRtOGNr9dKNHEt15jx9nuRN2RAIqfgvrWKr1k5Uc=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/racket-doc.zip";
      "stripRoot" = false;
    });
    "scribble-text-lib" = (fetchzip {
      "hash" = "sha256-ijCroSX00yuctgHBXbM2zgARG2dp3EA/F5KTRdHR8aw=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/scribble-text-lib.zip";
      "stripRoot" = false;
    });
    "class-iop-lib" = (fetchzip {
      "hash" = "sha256-34dkqCreXtMQe5Aw07sIscydwsXiHinbkraYJ+xCN0A=";
      "url" = "https://download.racket-lang.org/releases/8.9/pkgs/class-iop-lib.zip";
      "stripRoot" = false;
    });

    "corpix-strings" = "${cracketSource}/strings";
    "corpix-url" = "${cracketSource}/url";
    "corpix-db" = "${cracketSource}/db";
    "corpix-list" = "${cracketSource}/list";
    "corpix-http" = "${cracketSource}/http";
    "corpix-websocket" = "${cracketSource}/websocket";
    "corpix-multipart" = "${cracketSource}/multipart";
    "deta" = "${cracketSource}/deta";
    "corpix-os" = "${cracketSource}/os";
    "corpix-struct" = "${cracketSource}/struct";
    "corpix-bytes" = "${cracketSource}/bytes";
    "corpix-time" = "${cracketSource}/time";
    "corpix-configuration" = "${cracketSource}/configuration";
    "corpix-logging" = "${cracketSource}/logging";
    "corpix-syntax" = "${cracketSource}/syntax";
    "corpix-hex" = "${cracketSource}/hex";
    "corpix-css" = "${cracketSource}/css";
    "corpix-testbed" = "${cracketSource}/testbed";
  }) { inherit pkgs; }
}: let
  inherit (pkgs)
    stdenv
  ;
  inherit (pkgs.lib)
    makeBinPath
    makeLibraryPath
    concatMapStringsSep
    attrNames
  ;

  path = with pkgs; [
    bash
    nix
  ];
  libs = with pkgs; [
    sqlite
  ];
in stdenv.mkDerivation rec {
  name = "corpix-testbed";
  buildInputs = with pkgs; [
    racket
    findutils
    makeWrapper
    automake
    sqlite
    tzdata
  ];
  nativeBuildInputs = with pkgs; [makeWrapper];

  unpackPhase = ":";
  buildPhase = ''
    mkdir -p $out/pkgs
    export HOME=$(pwd)
    export TZDIR=${pkgs.tzdata}/share/zoneinfo

    ${(concatMapStringsSep "\n" (name: ''
      cp -r ${racketPackages.${name}} $out/pkgs/${name}
      find $out/pkgs/${name} -type d | xargs chmod 755
    '') (attrNames racketPackages))}

    cd $out/pkgs/tzinfo
    patch -p0 < ${./tzinfo.patch}
    cd -

    cd $out/pkgs/mime-type-lib
    ls -la
    patch -p1 < ${./mime-type.patch}
    cd -

    racket -l- pkg/dirs-catalog --link $HOME/catalog $out/pkgs
    raco pkg config --set catalogs file://$HOME/catalog
    export PLTADDONDIR=$HOME/.local/share/racket
  '';
  installPhase = ''
    raco pkg install --binary --batch --auto --user ${name}
    mkdir -p $out/bin
    raco exe -o $out/bin/testbed ${cracketSource}/testbed/testbed/testbed.rkt
    wrapProgram $out/bin/testbed \
                --prefix PATH ":" ${makeBinPath path} \
                --prefix LD_LIBRARY_PATH ":" ${makeLibraryPath libs} \
                --set TZDIR ${pkgs.tzdata}/share/zoneinfo
  '';
}
