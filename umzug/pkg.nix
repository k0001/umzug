{ mkDerivation, aeson, base, bytestring, containers, di, exceptions
, mtl, pipes, pipes-aeson, pipes-parse, stdenv, tasty, tasty-hunit
, text, time, transformers
}:
mkDerivation {
  pname = "umzug";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers di exceptions mtl pipes
    pipes-aeson pipes-parse text time transformers
  ];
  testHaskellDepends = [
    base bytestring di tasty tasty-hunit text time
  ];
  description = "Bidirectional migrations in your persistence backend of choice";
  license = stdenv.lib.licenses.bsd3;
}
