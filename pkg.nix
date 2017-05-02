{ mkDerivation, aeson, base, blaze-builder, bytestring, pipes
, pipes-aeson, pipes-parse, postgresql-simple, stdenv, tasty
, tasty-hunit, text, time, transformers
}:
mkDerivation {
  pname = "umzug";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base blaze-builder bytestring pipes pipes-aeson pipes-parse
    postgresql-simple text time transformers
  ];
  testHaskellDepends = [ base tasty tasty-hunit text time ];
  description = "Bidirectional migrations in your persistence backend of choice";
  license = stdenv.lib.licenses.bsd3;
}
