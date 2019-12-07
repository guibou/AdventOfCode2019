with import (builtins.fetchTarball {
url = https://github.com/NixOS/nixpkgs/archive/3140fa89c51.tar.gz;
sha256 = "18p0d5lnfvzsyfah02mf6bi249990pfwnylwhqdh8qi70ncrk3f8";
}) {};
with pkgs.haskellPackages;
developPackage {
  root = ./.;
}
