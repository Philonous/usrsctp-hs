with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "usrsctp";
  buildInputs = [ stack
                  gnumake
                  libtool
                  autoconf
                  automake
                ];
}
