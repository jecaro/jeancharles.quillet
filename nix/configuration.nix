{ pkgs, modulesPath, ... }:
let email = "jeancharles.quillet@gmail.com";
in
{
  console.keyMap = "fr";

  services.nginx.enable = true;

  services.nginx.virtualHosts."jeancharles.quillet.org" = {
    enableACME = true;
    forceSSL = true;
    root = "${pkgs.jeancharles-quillet}";
  };

  services.nginx.virtualHosts."quillet.org" = {
    globalRedirect = "jeancharles.quillet.org";
    enableACME = true;
    addSSL = true;
    serverAliases = [ "www.quillet.org" ];
  };

  services.nginx.virtualHosts."diverk.quillet.org" = {
    enableACME = true;
    forceSSL = true;
    locations."/".proxyPass = "http://localhost:8000";
  };

  security.acme.acceptTerms = true;
  security.acme.certs."diverk.quillet.org".email = email;
  security.acme.certs."jeancharles.quillet.org".email = email;
  security.acme.certs."quillet.org".email = email;

  networking.firewall = {
    allowedTCPPorts = [ 22 80 443 51413 ];
    allowedUDPPorts = [ 51413 ];

    extraCommands = ''
      ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -o tailscale0 -j MASQUERADE
      ${pkgs.iptables}/bin/iptables -t nat -A PREROUTING -p udp --dport 51413 -j DNAT --to-destination 100.108.81.35:51413
      ${pkgs.iptables}/bin/iptables -t nat -A PREROUTING -p tcp --dport 51413 -j DNAT --to-destination 100.108.81.35:51413
    '';
  };
  services.sshd.enable = true;

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

  services.tailscale = {
    enable = true;
    useRoutingFeatures = "server";
  };

  nix = {
    optimise.automatic = true;

    settings = {
      experimental-features = [ "nix-command" "flakes" ];
    };
    channel.enable = false;
    # Turn on flakes
    package = pkgs.nixVersions.stable; # or versioned attributes like nix_2_7
  };

  system.stateVersion = "23.05";
}
