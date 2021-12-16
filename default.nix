{ mkDerivation, base, lib, containers, mtl, vector, dlist, criterion }:
mkDerivation {
  pname = "advent";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers mtl vector dlist criterion ];
  testHaskellDepends = [ base ];
  description = "Advent of Code solutions";
  license = lib.licenses.mit;
}
