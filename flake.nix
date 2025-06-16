{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
      fakeGit = pkgs:
        if builtins.hasAttr "rev" self then
          pkgs.writeShellScriptBin "git" ''
            echo ${self.rev}
          ''
        else
          pkgs.git;
    in
    {
      overlay = (final: prev:
        {
          cache = final.callPackage ./nix/cache.nix { };

          site = final.haskell.lib.addBuildTool
            (final.haskellPackages.callCabal2nix "site" ./. { })
            (fakeGit final);

          jeancharles-quillet = final.stdenv.mkDerivation {
            name = "jeancharles-quillet";
            src = ./.;

            LC_ALL = "C.UTF-8";

            buildInputs = [ final.site ];
            buildPhase = ''
              ${final.site}/bin/site build --cache-dir ${final.cache}
            '';

            installPhase = ''
              cp -r _site $out
            '';
          };
        });

      packages = forAllSystems (system: {
        cache = nixpkgsFor.${system}.cache;
        site = nixpkgsFor.${system}.site;
        jeancharles-quillet = nixpkgsFor.${system}.jeancharles-quillet;
      });

      defaultPackage = forAllSystems (system: self.packages.${system}.jeancharles-quillet);

      checks = self.packages;

      devShell = forAllSystems (system:
        let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in
        haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.site ];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
        });

    };
}
