FROM ubuntu:16.04

# Bring the system up to date and install prerequisite system libraries
RUN apt-get update
RUN apt-get install openssl libssl1.0.0

# Install the syscat executable
ADD syscat /syscat

# Start up the application by default
CMD ["/syscat"]
