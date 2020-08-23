with import <nixpkgs> {};

let
    syscat_deriv = stdenv.mkDerivation rec {
        name = "syscat";
        builder = "${bash}/bin/bash";
        args = [ ./nix-builder.sh ];
        inherit coreutils openssl libyaml;
        system = builtins.currentSystem;
        schemapath = ../../src/schemas;
        syscatpath = ./syscat;
    };

    # Prepare the value of LD_LIBRARY_PATH
    ld_path = stdenv.lib.makeLibraryPath [
        pkgs.openssl
        pkgs.libyaml
    ];

    entrypoint = writeScript "entrypoint.sh" ''
    #!${stdenv.shell}
    export LD_LIBRARY_PATH=${ld_path}
    exec $@
    '';

in
pkgs.dockerTools.buildImage {
    name = "equill/syscat";
    tag = "0.2.0a24";
    created = "now";

    contents = [ syscat_deriv file bash coreutils ];

    config = {
        Cmd = [ "syscat" ];
        Entrypoint = [ entrypoint ];
        ExposedPorts = {
            "4949/tcp" = {};
        };
        WorkingDir = "/opt";
    };
}
