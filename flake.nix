{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (
    system:
      let
        pkgs = import nixpkgs { inherit system; };

        haskellLib = pkgs.haskell.lib;

        haskellPackages = pkgs.haskell.packages.ghc912.override {
          overrides = hself: hsuper: {
            ghc-tcplugin-api = hself.callHackageDirect {
              pkg = "ghc-tcplugin-api";
              ver = "0.19.0.0";
              sha256 = "142q8cx6kmn3hzs3542m0yys0kg1bzy75w8rnnpnws36d51vaffs";
            } {};

            # Plugin test suites load the plugin from a not-yet-installed
            # package DB, which fails inside the nix sandbox. Skip them.
            ghc-typelits-extra = haskellLib.dontCheck (hself.callHackageDirect {
              pkg = "ghc-typelits-extra";
              ver = "0.5.4";
              sha256 = "0zdvyanwmvrwc1ahplif5y9572a6k92l7gjvdhvbjpv4ldq6sl6r";
            } {});

            ghc-typelits-knownnat = haskellLib.dontCheck (hself.callHackageDirect {
              pkg = "ghc-typelits-knownnat";
              ver = "0.8.4";
              sha256 = "1s49mmdsz2s8836y3zmmsam8khybbbnfyrf3pam6izkwy990q9iz";
            } {});

            ghc-typelits-natnormalise = haskellLib.dontCheck (hself.callHackageDirect {
              pkg = "ghc-typelits-natnormalise";
              ver = "0.9.6";
              sha256 = "0yg72pm3sgm47gh7zr3vig29bdfdx3kjpicxarhizb49i15rymkb";
            } {});

            checked-literals = haskellLib.overrideCabal
              (hself.callCabal2nix "checked-literals" ./. {})
              (drv: {
                # The test suite spawns a fresh `ghc` subprocess that loads CheckedLiterals
                # as a plugin. Point GHC_PACKAGE_PATH at the cabal inplace db so the
                # just-built library is visible.
                preCheck = (drv.preCheck or "") + ''
                  export NIX_GHC_PACKAGE_PATH_FOR_TEST="$PWD/dist/package.conf.inplace:$packageConfDir:"
                '';
              });
          };
        };
      in {
        packages.default = haskellPackages.checked-literals;

        devShells.default = haskellPackages.shellFor {
          packages = p: [ p.checked-literals ];

          nativeBuildInputs = [
            haskellPackages.cabal-install
            haskellPackages.cabal-gild
            haskellPackages.fourmolu

            # https://discourse.nixos.org/t/non-interactive-bash-errors-from-flake-nix-mkshell/33310
            pkgs.bashInteractive
          ];
        };
      }
  );
}
