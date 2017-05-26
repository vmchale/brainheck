{ mkDerivation, base, containers, lens, megaparsec, mtl
, optparse-applicative, recursion-schemes, stdenv, text, vector
}:
mkDerivation {
  pname = "brainheck";
  version = "0.1.0.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers lens megaparsec mtl recursion-schemes text vector
  ];
  executableHaskellDepends = [ base optparse-applicative text ];
  homepage = "https://github.com/vmchale/brainheck#readme";
  description = "Brainh*ck interpreter in haskell";
  license = stdenv.lib.licenses.bsd3;
}
