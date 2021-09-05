{
  description = "org2jekyll flake";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
      follows = "nix/nixpkgs";
    };

    flake-utils = {
      type = "github";
      owner = "numtide";
      repo = "flake-utils";
      ref = "master";
    };
  };

  outputs = { self, nix, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let lib = nixpkgs.lib;
            pkgs = nixpkgs.legacyPackages.${system};
            pname = "org2jekyll";
            version = "0.2.7";
        in rec {
          packages."${system}" = rec {
            org2jekyll = pkgs.stdenv.mkDerivation {
              inherit pname version;
              src = ./.;
              buildInputs = with pkgs.emacs.pkgs; [
                emacs s dash htmlize
              ];
              unpackPhase = ''
                cp $src/org2jekyll.el .
              '';
              buildPhase = ''
                emacs -L . --batch -f batch-byte-compile *.el
              '';
              installPhase =
                let install-dir = "$out/share/emacs/site-lisp/elpa/${pname}-${version}/"; in
                ''
                mkdir -p ${install-dir}
                cp -v *.el *.elc ${install-dir}
              '';

              doCheck = false;

              meta = {
                description = "Minor mode to publish org-mode post to jekyll without specific yaml.";
                homepage = https://github.com/ardumont/org2jekyll/;
                license = lib.licenses.gpl2;
                maintainers = with lib.maintainers; [ ardumont ];
              };
            };
          };

          devShell = import ./shell.nix { pkgs = pkgs // packages."${system}"; };

          defaultPackage = packages."${system}".org2jekyll;
        });
}
