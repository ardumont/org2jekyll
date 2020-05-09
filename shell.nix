with import <nixpkgs> {};

let sources = import ./nix/sources.nix;
    pkgs = import sources.nixpkgs {};
    jekyll-env = bundlerEnv {
    name = "jekyll-tryout";
    inherit ruby;
    gemdir = ./.;
  };

in stdenv.mkDerivation {
  name = "org2jekyll-env";
  buildInputs = [
    # emacs-lisp testing
    pkgs.cask
    # jekyll instance to check manually against
    zlib
    ruby
    jekyll
    bundler
    bundix
    gitAndTools.gitFull
  ];
  src = null;
}
