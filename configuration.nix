{ pkgs, modulesPath, ... }:
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
    serverAliases = [ "www.quillet.org" "kodreizh.com" "www.kodreizh.com" ];
  };

  security.acme.acceptTerms = true;
  security.acme.certs."jeancharles.quillet.org".email =
    "jeancharles.quillet@gmail.com";
  security.acme.certs."quillet.org".email = "jeancharles.quillet@gmail.com";

  networking.firewall.allowedTCPPorts = [ 22 80 443 ];
  services.sshd.enable = true;

  system.stateVersion = "22.11";
}
