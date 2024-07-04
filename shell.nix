{ defaultPlatformProject ? import ./default.nix {} }:

  defaultPlatformProject.webdriver-w3c.shellFor {
    # Include only the *local* packages of your project.
    packages = ps: with ps; [
      webdriver-w3c
      #pkgb
    ];

    # Builds a Hoogle documentation index of all dependencies,
    # and provides a "hoogle" command to search the index.
    withHoogle = true;

    # You might want some extra tools in the shell (optional).

    # Some common tools can be added with the `tools` argument
    #tools = { cabal = "latest"; hlint = "latest"; };
    tools = { cabal = defaultPlatformProject.pkgs.haskell-nix.cabal-install.ghc8107.version;
              hasktags = "latest";
              haskell-language-server = "latest";
            };
    # See overlays/tools.nix for more details

    # Some you may need to get some other way.
    buildInputs = with defaultPlatformProject.pkgs;
      #[ ghcid lorri niv ];
      [ ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
}
