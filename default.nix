{ pkgs ? import (fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/61db79b0c6b838d9894923920b612048e1201926.tar.gz";
  sha256 = "sha256-h5bRFy9bco+8QcK7rGoOiqMxMbmn21moTACofNLRMP4=";
}) { } }:

pkgs.stdenv.mkDerivation rec {
  name = "coqbot";
  src = null;
  buildInputs = with pkgs.ocamlPackages; [ # Compiler and dev tools
    ocaml
    findlib
    dune_3
    utop
    pkgs.ncurses
    merlin
    ocaml-lsp
    pkgs.ocamlformat
    pkgs.nixfmt
    pkgs.nodePackages.get-graphql-schema
    # Direct dependencies
    base
    camlzip
    cohttp
    cohttp-lwt-unix
    ohex
    iso8601
    mirage-crypto
    mirage-crypto-rng
    yojson
    graphql_ppx
    toml
    eqaf
    x509
    digestif
    ppx_expect
    odoc
    alcotest
  ];

  shellHook = ''
    export OCAMLFORMAT_LOCATION=${pkgs.ocamlformat}
  '';
}
