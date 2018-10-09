{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, containers, lens
      , lens-aeson, roundtrip, scientific, stdenv, text
      , unordered-containers, vector
      }:
      mkDerivation {
        pname = "roundtrip-aeson";
        version = "0.2.0.0";
        sha256 = "0m96447l2m0y4aapil077xpnzlkjla0yp2bzajfijik9gkjbiih4";
        libraryHaskellDepends = [
          aeson base bytestring containers lens lens-aeson roundtrip
          scientific text unordered-containers vector
        ];
        testHaskellDepends = [
          aeson base bytestring lens-aeson roundtrip text vector
        ];
        homepage = "https://github.com/anchor/roundtrip-aeson";
        description = "Un-/parse JSON with roundtrip invertible syntax definitions";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
