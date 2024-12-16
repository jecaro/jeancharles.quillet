{ pkgs, modulesPath, ... }:
let email = "jeancharles.quillet@gmail.com";
in
{
  console.keyMap = "fr";

  services = {
    nginx = {
      enable = true;

      virtualHosts."jeancharles.quillet.org" = {
        enableACME = true;
        forceSSL = true;
        root = "${pkgs.jeancharles-quillet}";
      };

      virtualHosts."quillet.org" = {
        globalRedirect = "jeancharles.quillet.org";
        enableACME = true;
        addSSL = true;
        serverAliases = [ "www.quillet.org" ];
      };

      virtualHosts."diverk.quillet.org" = {
        enableACME = true;
        forceSSL = true;
        locations."/".proxyPass = "http://localhost:8000";
      };
    };

    sshd.enable = true;

    tailscale = {
      enable = true;
      useRoutingFeatures = "server";
    };
  };

  security.acme = {
    acceptTerms = true;
    certs = {
      "diverk.quillet.org".email = email;
      "jeancharles.quillet.org".email = email;
      "quillet.org".email = email;
    };
  };

  systemd.services.diverk = {
    description = "Diverk";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.diverk}/backend";
      WorkingDirectory = "${pkgs.diverk}";
      Restart = "always";
      RestartSec = 10;
    };
  };

  networking.firewall = {
    allowedTCPPorts = [ 22 80 443 51413 ];
    allowedUDPPorts = [ 51413 ];

    extraCommands = ''
      ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -o tailscale0 -j MASQUERADE
      ${pkgs.iptables}/bin/iptables -t nat -A PREROUTING -p udp --dport 51413 -j DNAT --to-destination 100.108.81.35:51413
      ${pkgs.iptables}/bin/iptables -t nat -A PREROUTING -p tcp --dport 51413 -j DNAT --to-destination 100.108.81.35:51413
    '';
  };

  nix = {
    optimise.automatic = true;
    channel.enable = false;
    settings.experimental-features = [ "nix-command" "flakes" ];
    # Turn on flakes
    package = pkgs.nixVersions.stable; # or versioned attributes like nix_2_7
  };

  system.stateVersion = "23.05";
}
