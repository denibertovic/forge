let
  sources = import ./nix/sources.nix;
  # Disable tests for these packages
  dontCheckPackages = [
  ];

  # Jailbreak these packages
  doJailbreakPackages = [
  ];

  # Disable haddocks for these packages
  dontHaddockPackages = [
  ];

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =
        let
          generatedOverrides = haskellPackagesNew: haskellPackagesOld:
            let
              toPackage = file: _: {
                name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;

                value = haskellPackagesNew.callPackage (./. + "/nix/overrides/${file}") { };
              };

            in
              pkgs.lib.mapAttrs' toPackage ((builtins.readDir ./nix/overrides));

          makeOverrides =
            function: names: haskellPackagesNew: haskellPackagesOld:
              let
                toPackage = name: {
                  inherit name;

                  value = function haskellPackagesOld.${name};
                };

            in
              builtins.listToAttrs (map toPackage names);

          composeExtensionsList =
            pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {});

          # More exotic overrides go here
          manualOverrides = haskellPackagesNew: haskellPackagesOld: {
          };

          # Aaaand finally we need to add this project to the mix
          thisProject = haskellPackagesNew: haskellPackagesOld: {
              project = haskellPackagesNew.callPackage (./project.nix) { };
          };
        in
          pkgs.haskellPackages.override {
            overrides = composeExtensionsList [
              generatedOverrides
              (makeOverrides pkgs.haskell.lib.dontCheck   dontCheckPackages  )
              (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreakPackages)
              (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddockPackages)
              thisProject
              manualOverrides
            ];
          };
    };
  };
  # This is VERY important as we PIN nixpkgs so we're not dependant on what the
  # global nix-channel is set to on your host system
  pkgs = import sources.nixpkgs { inherit config; };

in
  { project = pkgs.haskellPackages.project;
  }
