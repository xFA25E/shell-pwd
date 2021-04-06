with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "shell-pwd-env";
  buildInputs = [ eldev ];
}
