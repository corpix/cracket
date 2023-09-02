{ config, pkgs, lib, ... }:
let
  inherit (builtins)
    toJSON
  ;
  inherit (pkgs)
    writeText
  ;
  inherit (lib)
    mkIf
    mkOption
    types
    fold
    attrNames
    optionalString
    concatStringsSep
    concatMapStringsSep
  ;

  name = "firewalld";
  cfg = config.services.${name};

  attrsToRacket = attrs: concatMapStringsSep "\n "
    (name: "(${name} . ${toJSON attrs.${name}})")
    (attrNames attrs);
  mkConfig = attrs: writeText "config.rkt"
    "(${attrsToRacket attrs})";
in {
  options.services.${name} = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
    configuration = mkOption {
      type = types.nullOr (types.attrsOf types.anything);
      default = null;
    };
  };

  config = mkIf cfg.enable {
    users.groups.${name} = {};
    users.users.${name} = {
      description = "Firewall daemon user";
      isSystemUser = true;
      group = name;
      extraGroups = ["systemd-journal"];
    };

    systemd.services.${name} = {
      description = "Firewall daemon";
      wantedBy = ["multi-user.target"];
      after = ["network.target"];

      serviceConfig = {
        Type = "simple";
        User = name;
        ExecStart = "${pkgs.firewalld}/bin/firewalld ${optionalString (cfg.configuration != null) "-c ${mkConfig cfg.configuration}"}";
        AmbientCapabilities = "CAP_NET_ADMIN";
        Restart = "on-failure";
        RestartSec = 5;
      };
    };
  };
}
