{ pkgs, ... }:

let org2jekyll-emacs = pkgs.emacsWithPackages (epkgs:
      (with epkgs.melpaStablePackages; [
        pkgs.org2jekyll
        dash
        s
        htmlize
      ]));
in pkgs.stdenv.mkDerivation {
  name = "org2jekyll-env";
  buildInputs = with pkgs; [
    org2jekyll-emacs
    # emacs-lisp testing
    cask
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
