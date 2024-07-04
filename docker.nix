{ nativePkgs ? (import ./default.nix {}).pkgs,
crossBuildProject ? import ./cross-build.nix {} }:
nativePkgs.lib.mapAttrs (_: prj:
with prj.webdriver-w3c;
let
  executable = webdriver-w3c.webdriver-w3c.components.exes.webdriver-w3c;
  binOnly = prj.pkgs.runCommand "webdriver-w3c-bin" { } ''
    mkdir -p $out/bin
    cp ${executable}/bin/webdriver-w3c $out/bin
    ${nativePkgs.nukeReferences}/bin/nuke-refs $out/bin/webdriver-w3c
  '';
in { 
  webdriver-w3c-image = prj.pkgs.dockerTools.buildImage {
  name = "webdriver-w3c";
  tag = executable.version;
  contents = [ binOnly prj.pkgs.cacert prj.pkgs.iana-etc ];
  config.Entrypoint = "webdriver-w3c";
  config.Cmd = "--help";
  };
}) crossBuildProject
