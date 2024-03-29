{ source-repo-override ? { } }:
########################################################################
# default.nix -- The top-level nix build file for traceability.
#
# This file defines various attributes that are used for building and
# developing traceability.
#
########################################################################

let
  # Here a some of the various attributes for the variable 'packages':
  #
  # { pkgs
  #   traceability: {
  #     haskell: {
  #       project # The Haskell project created by haskell-nix.project
  #       packages # All the packages defined by our project, including dependencies
  #       projectPackages # Just the packages in the project
  #     }
  #     hlint
  #     cabal-install
  #     stylish-haskell
  #     haskell-language-server
  #   }
  # }
  packages = import ./nix { inherit source-repo-override; };

  inherit (packages) pkgs traceability;
  project = traceability.haskell.project;
in
{
  inherit pkgs traceability;

  inherit project;
}
