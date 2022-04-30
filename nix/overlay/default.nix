self: prev: 
with prev.lib; mapAttrs' (attr: _: { name = removeSuffix ".nix" attr; value = ./.  + "/${attr}"; }) (filterAttrs (attr: _: attr != "default.nix" ) (builtins.readDir ./.)) 
