with import <nixpkgs> {};

stdenv.mkDerivation rec {
    name = "syscat";

    buildInputs = [
        pkgs.gcc_multi
        pkgs.gcc
        pkgs.libyaml
        pkgs.openssl
        pkgs.sbcl
        pkgs.neo4j
        pkgs.bash
    ];

    env = buildEnv {
        name = name;
        paths = buildInputs;
    };

    LD_LIBRARY_PATH = stdenv.lib.makeLibraryPath [
        pkgs.openssl
        pkgs.libyaml
    ];

}
