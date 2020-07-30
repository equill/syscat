export PATH="$coreutils/bin"
mkdir -p $out/schemas
cp -r $schemapath/*.yaml $out/schemas/
mkdir -p $out/bin
cp $syscatpath $out/bin/syscat
