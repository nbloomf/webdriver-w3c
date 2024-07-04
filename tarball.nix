{ nativePkgs ? (import ./default.nix {}).pkgs,
crossBuildProject ? import ./cross-build.nix {} }:
nativePkgs.lib.mapAttrs (_: prj:
with prj.webdriver-w3c;
let
  executable = webdriver-w3c.webdriver-w3c.components.exes.webdriver-w3c;
  binOnly = prj.pkgs.runCommand "webdriver-w3c-bin" { } ''
    mkdir -p $out/bin
    cp -R ${executable}/bin/* $out/bin/
    ${nativePkgs.nukeReferences}/bin/nuke-refs $out/bin/webdriver-w3c
  '';

  tarball = nativePkgs.stdenv.mkDerivation {
    name = "webdriver-w3c-tarball";
    buildInputs = with nativePkgs; [ zip ];

    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out/
      zip -r -9 $out/webdriver-w3c-tarball.zip ${binOnly}
    '';
  };
in {
 webdriver-w3c-tarball = tarball;
}
) crossBuildProject
