{ mkDerivation
, base
, hlint
, haskell-language-server
, lib
, containers
, mtl
, vector
, dlist
, criterion
, split
, hashtables
, hashable
}:

mkDerivation {
  pname = "advent";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base
    containers
    mtl
    vector
    dlist
    criterion
    split
    hashtables
    hashable
  ];
  testHaskellDepends = [ base ];
  buildTools = [ hlint haskell-language-server ];
  description = "Advent of Code solutions";
  license = lib.licenses.mit;
}
