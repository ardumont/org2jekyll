with import <nixpkgs> {};

let sources = import ./nix/sources.nix;
    pkgs = import sources.nixpkgs {};
    org2jekyll-emacs = emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
      dash-functional
      s
      htmlize
    ]));
in stdenv.mkDerivation {
  name = "org2jekyll-env";
  buildInputs = [
    org2jekyll-emacs
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
