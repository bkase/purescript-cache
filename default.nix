with import <nixpkgs> { };

# TODO: This is not PURE. We want purity

stdenv.mkDerivation rec {
  name = "psc-play-${version}";
  version = "0.1.0";

  buildInputs = [
    nodejs
    nodePackages.npm
    nodePackages.bower
    haskellPackages.purescript
  ];

  shellHook = ''
    [ $(node -p "try{require('pulp/package.json').version}catch(e){}") != "9.0.0" ] && npm install pulp@9.0.0
    export PATH=$PATH:./node_modules/.bin
  '';
}
