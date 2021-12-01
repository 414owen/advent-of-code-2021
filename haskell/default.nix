{ mkDerivation, base, lib, tasty, tasty-hedgehog, tasty-hunit }:
mkDerivation {
  pname = "rwh";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base tasty tasty-hedgehog tasty-hunit ];
  description = "Real World Haskell solutions";
  license = lib.licenses.mit;
}
