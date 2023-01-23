{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev:
        {
          site = final.haskellPackages.callCabal2nix "site" ./. { };
          jeancharles-quillet = final.stdenv.mkDerivation {
            name = "jeancharles-quillet";
            src = ./.;

            LC_ALL = "C.UTF-8";

            buildInputs = [ final.site ];
            buildPhase = ''
              ${final.site}/bin/site build
            '';

            doCheck = true;
            checkPhase = ''
              ${final.site}/bin/site check
            '';

            installPhase = ''
              cp -r _site $out
            '';
          };
        });

      packages = forAllSystems (system: {
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
          # Change the prompt to show that you are in a devShell
          shellHook = "export PS1='\\[\\e[1;34m\\]dev > \\[\\e[0m\\]'";
        });

      # Build
      # nixos-rebuild build --option sandbox false --flake .#website-prod
      # nix build --option sandbox false ./#nixosConfigurations.website-prod.config.system.build.toplevel
      # Deploy
      # nixos-rebuild switch --option sandbox false --flake .#website-prod --target-host quillet.org --build-host localhost
      nixosConfigurations.website-prod = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ({ ... }: { nixpkgs.overlays = [ self.overlay ]; })
          ./configuration.nix
        ]
        ++
        (nixpkgs.lib.optional (builtins.pathExists ./do-userdata.nix) ./do-userdata.nix ++ [
          (nixpkgs + "/nixos/modules/virtualisation/digital-ocean-config.nix")
        ]);
      };

      # Build
      # nix build --option sandbox false ./#nixosConfigurations.website-vm.config.system.build.vm
      # Run the VM forwarding the ports
      # QEMU_NET_OPTS="hostfwd=tcp::2222-:22,hostfwd=tcp::8888-:80,hostfwd=tcp::4444-:443" ./result/bin/run-nixos-vm
      nixosConfigurations.website-vm = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ({ ... }: { nixpkgs.overlays = [ self.overlay ]; })
          ./configuration.nix
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
