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

  networking.firewall.allowedTCPPorts = [ 22 80 443 ];
  services.sshd.enable = true;

  system.stateVersion = "23.05";

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
}
