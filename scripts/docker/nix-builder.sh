export PATH="$coreutils/bin"
mkdir -p $out/bin
mkdir -p $out/schemas
cp -r $schemapath/*.yaml $out/schemas/
cp $syscatpath $out/bin/syscat
