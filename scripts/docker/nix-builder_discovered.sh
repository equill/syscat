export PATH="$coreutils/bin"
mkdir -p $out/schemas
cp -r $schemapath/common/*.yaml $out/schemas/
cp -r $schemapath/discovered/*.yaml $out/schemas/
mkdir -p $out/bin
cp $syscatpath $out/bin/syscat
