{ mkDerivation, base, lib, containers }:
mkDerivation {
  pname = "advent";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base, containers ];
  testHaskellDepends = [ base ];
  description = "Advent of Code solutions";
  license = lib.licenses.mit;
}
