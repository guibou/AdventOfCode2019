with import (builtins.fetchTarball {
url = https://github.com/NixOS/nixpkgs/archive/3140fa89c51.tar.gz;
sha256 = "18p0d5lnfvzsyfah02mf6bi249990pfwnylwhqdh8qi70ncrk3f8";
}) {};
with pkgs.haskellPackages;
developPackage {
  root = ./.;

  overrides = self: super:
    {
      weigh = super.callHackageDirect {
        pkg = "weigh";
        ver = "0.0.16";
        sha256 = "0icdyvxxi7493ch8xlpwn024plspbsdssxmcy5984yar298z8hcw";
      } {};

      besout = haskell.lib.doJailbreak super.besout;
    };
}
