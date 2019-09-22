with import <nixpkgs> {};

let
    syscat_deriv = stdenv.mkDerivation rec {
        name = "syscat";
        builder = "${bash}/bin/bash";
        args = [ ./nix-builder_discovered.sh ];
        inherit coreutils openssl libyaml;
        system = builtins.currentSystem;
        schemapath = ../../src/schemas;
        syscatpath = ./syscat_discovered;
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
    name = "equill/syscat_discovered";
    tag = "0.1.5";

    contents = syscat_deriv;

    config = {
        Cmd = [ "syscat" ];
        Entrypoint = [ entrypoint ];
        ExposedPorts = {
            "4949/tcp" = {};
        };
        WorkingDir = "/opt";
    };
}
