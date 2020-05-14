FROM haskell:8.8.3

### Install libpq-dev for postgres haskell lib to be built & entr for detecting changes and restarting stack 
RUN apt-get update \
     && apt-get -y install libpq-dev entr \
     && rm -rf /var/lib/apt/lists/*

WORKDIR /root/

## Prebuild dependencies 
COPY stack.* package.yaml /root/
RUN stack build --only-dependencies


COPY ./entrypoint.sh /opt/entrypoint.sh
RUN chmod +x /opt/entrypoint.sh

COPY . /root

ENTRYPOINT ["/opt/entrypoint.sh"]
