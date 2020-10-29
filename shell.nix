with import <nixpkgs> {};

stdenv.mkDerivation rec {
    name = "syscat";

    buildInputs = [
        pkgs.libyaml
        pkgs.openssl
        pkgs.sbcl
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

    shellHook = "export PS1='\n\\[\\033[01;32m\\][nix syscat] \\w\\$\\[\\033[00m\\] '";

}
