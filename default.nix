{ pkgs ? import <nixpkgs> {}, ... }:
(import ./overlay.nix { inherit pkgs; }).cracket
