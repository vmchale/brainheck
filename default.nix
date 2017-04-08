{ mkDerivation, base, containers, lens, megaparsec, mtl
, optparse-applicative, primitive, recursion-schemes, stdenv, text
, vector
}:
mkDerivation {
  pname = "brainheck";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers lens megaparsec mtl optparse-applicative primitive
    recursion-schemes text vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/vmchale/brainheck#readme";
  description = "Brainh*ck interpreter in haskell";
  license = stdenv.lib.licenses.bsd3;
}
