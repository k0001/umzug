{ stdenv, postgresql, mkDerivation
, aeson, base, blaze-builder, bytestring, containers
, di, ex-pool, exceptions, pipes, pipes-aeson, pipes-parse, pipes-safe
, postgresql-simple, tasty, tasty-hunit, text, time
, transformers
}:
mkDerivation {
  pname = "umzug";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base blaze-builder bytestring containers di exceptions pipes
    pipes-aeson pipes-parse pipes-safe postgresql-simple text time
    transformers
  ];
  testHaskellDepends = [
    base bytestring di ex-pool postgresql-simple tasty tasty-hunit text time
  ];
  description = "Bidirectional migrations in your persistence backend of choice";
  license = stdenv.lib.licenses.bsd3;
  preCheck = ''
    echo "Starting PostgreSQL server"
    pg_data_dir=$(mktemp -d)
    ${postgresql}/bin/initdb -D $pg_data_dir
    echo "log_statement = 'all'" >> $pg_data_dir/postgresql.conf
    ${postgresql}/bin/pg_ctl -D $pg_data_dir start
    while [ ! -e $pg_data_dir/postmaster.pid ]; do sleep 1; done
    echo "Creating PostgreSQL databases"
    ${postgresql}/bin/psql \
      -h 127.0.0.1 -p 5432 -d template1 \
      -c "CREATE ROLE umzug PASSWORD 'umzug' LOGIN;"
    ${postgresql}/bin/createdb \
      -h 127.0.0.1 -p 5432 -T template0 -E UTF-8 -O umzug umzug
    export UMZUG_TEST_PG=postgresql://umzug:umzug@127.0.0.1:5432/umzug
  '';
}
