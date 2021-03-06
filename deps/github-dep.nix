{ 
nixpkgs ? import <nixpkgs> {},
spec,
extra ? {}
}:
  nixpkgs.pkgs.fetchFromGitHub (
    let json = builtins.fromJSON (builtins.readFile spec);
    in { 
      inherit (json) owner repo rev sha256;
      private = json.private or false;
    } // extra
  )