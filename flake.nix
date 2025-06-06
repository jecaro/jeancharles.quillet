{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
  inputs.diverk-src =
    {
      url = "github:jecaro/diverk";
      flake = false;
    };
  outputs = { self, diverk-src, nixpkgs }:
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

          # Diverk uses obelisk infrastructure to build. obelisk is not pure yet
          # so we need to build with `--impure`
          diverk = (import diverk-src { system = final.system; }).linuxExe;
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

      # Build
      # nixos-rebuild build --impure --flake .#website-prod
      # nix build --impure ./#nixosConfigurations.website-prod.config.system.build.toplevel
      # Deploy
      # nixos-rebuild switch --impure --flake .#website-prod --target-host quillet.org
      nixosConfigurations.website-prod = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ({ ... }: { nixpkgs.overlays = [ self.overlay ]; })
          ./nix/configuration.nix
        ]
        ++
        (nixpkgs.lib.optional (builtins.pathExists ./do-userdata.nix) ./do-userdata.nix ++ [
          (nixpkgs + "/nixos/modules/virtualisation/digital-ocean-config.nix")
        ]);
      };

      # Build
      # nix build --impure ./#nixosConfigurations.website-vm.config.system.build.vm
      # Run the VM forwarding the ports
      # QEMU_NET_OPTS="hostfwd=tcp::2222-:22,hostfwd=tcp::8888-:80,hostfwd=tcp::4444-:443" ./result/bin/run-nixos-vm
      nixosConfigurations.website-vm = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ({ ... }: { nixpkgs.overlays = [ self.overlay ]; })
          ./nix/configuration.nix
          ({ ... }: {
            virtualisation.vmVariant.virtualisation.graphics = false;
            users.users.guest = {
              isNormalUser = true;
              extraGroups = [ "wheel" ];
              initialPassword = "";
            };
            security.sudo.wheelNeedsPassword = false;
          })
        ];

      };
    };
}
