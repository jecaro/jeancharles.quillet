{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
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
      fakeGit = pkgs: pkgs.writeShellScriptBin "git" ''
        echo ${self.rev or "dirty"}
      '';
    in
    {
      overlay = (final: prev:
        {
          cache = final.linkFarm "cache" [
            # Basic nix VM for just anything
            {
              name = "a10bb77486b482ea0d88e46e92443601674ff60ca948a6850e90510359edcf46";
              path = builtins.fetchurl
                {
                  url = "https://raw.githubusercontent.com/jecaro/simple-nix-vm/94265f73fed25ed73624d4635865aa90b91405dd/vm.nix";
                  sha256 = "sha256:0sgndz3j2qzl0mmaz5rryjbv5id9l7h5qzjz1rmp47b0pillqfjn";
                };
            }
            # Deploying a static website with nix
            {
              name = "1b0e4f37e2a7ddca40c14883b131af03e78910504a95d759915182141fc27e12";
              path = builtins.fetchurl
                {
                  url = "https://raw.githubusercontent.com/jecaro/jeancharles.quillet/81c7c9832ab0ea11f881dbc840db07e3cf34f3db/flake.nix";
                  sha256 = "sha256:01d739bqg1gn58l27px7a3pr086sx557h7yz2xvq2lipa3c3nasv";
                };
            }
            {
              name = "1ad6a29b7b1ac4f52e7cfa1e993f0b3995cb6560b4c9ae0e92862acb7d6abd73";
              path = builtins.fetchurl
                {
                  url = "https://raw.githubusercontent.com/jecaro/jeancharles.quillet/6bffc9b5a949ddb831b33930b4d5f25cfbcd814e/configuration.nix";
                  sha256 = "sha256:00qxpnmnmpcv74s3j9qrmhm1vjzr90r374qni8jw6ilsicy5s18w";
                };
            }
            {
              name = "304cdc707d98ac430235d0dec7bbf1c8867954fab0c89fa185592bcd273dc6ce";
              path = builtins.fetchurl
                {
                  url = "https://raw.githubusercontent.com/jecaro/jeancharles.quillet/6bffc9b5a949ddb831b33930b4d5f25cfbcd814e/flake.nix";
                  sha256 = "sha256:0zfn6rq9p9b3ivm40bia92b16x3msc21710g78idxggqdzsfs1xz";
                };
            }
            # Writing an Android app in Haskell
            {
              name = "f5ab2eef8cdec243828a391109b780c1c9cb66a2ece8562b3eaa583299db440a";
              path = builtins.fetchurl
                {
                  url = "https://raw.githubusercontent.com/jecaro/diverk/e777f2d44ed4ecdb08ed8f2b3c832d327bea7611/shell.nix";
                  sha256 = "sha256:0i8iz1qrlqf53vlnskgnhbb4sk25d1dgvf0zavmqfrj0h3cqck93";
                };
            }
            {
              name = "462fec01f8f32382a601569f1d9596498e506fdd1a71e66051d3c095a2bffccf";
              path = builtins.fetchurl
                {
                  url = "https://raw.githubusercontent.com/jecaro/diverk/78ff0683f0b77d40d907ff19f4a9771c5406957a/default.nix";
                  sha256 = "sha256:1dsliwx6x1n3zmf2icpnyy3gzg9rmyy613smcxvrmxj616lij6dm";
                };
            }
          ];

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
          # Change the prompt to show that you are in a devShell
          shellHook = "export PS1='\\[\\e[1;34m\\]dev > \\[\\e[0m\\]'";
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
          ./configuration.nix
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
