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
  # GEM_HOME = "./gems";  # Do not install gems in user's home (default behavior)
  BUNDLE_PATH = "../gems";
  BUNDLE_DISABLE_SHARED_GEMS = 1;
  src = null;
}
