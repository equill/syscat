FROM ubuntu:16.04

# Bring the system up to date and install prerequisite system libraries
RUN apt-get update
RUN apt-get install openssl=1.0.2g-1ubuntu4 libssl1.0.0=1.0.2g-1ubuntu4 libyaml-0.2=0.1.6-3

# Install the syscat executable
ADD syscat /syscat

# Install the schema file
ADD schema.yaml /schema.yaml

# Start up the application by default
CMD ["/syscat"]
