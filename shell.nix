{ sources ? import ./nix/sources.nix }:
let pkgs = import sources.nixpkgs { };
in pkgs.mkShell {
  nativeBuildInputs = with pkgs;
    let sbtWithJava21 = sbt.override { jre = corretto21; };
    in [ sbtWithJava21 metals ];
}
