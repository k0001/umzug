{ mkDerivation, base, blaze-builder, postgresql-simple, stdenv
, text, time, transformers
}:
mkDerivation {
  pname = "umzug";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base blaze-builder postgresql-simple text time transformers
  ];
  description = "Bidirectional migrations in your persistence backend of choice";
  license = stdenv.lib.licenses.bsd3;
}
