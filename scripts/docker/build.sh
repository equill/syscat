cp ../../src/schema.yaml ./
mv ../syscat ./
docker build -t equill/syscat:latest -t equill/syscat:0.2 .
