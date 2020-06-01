with import <nixpkgs> {};
mkShell {
    buildInputs = [
        rustc
        rustfmt
        shellcheck
    ];
    shellHook = ''
        . .shellhook
    '';
}
