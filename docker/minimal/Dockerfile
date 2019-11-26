FROM emscsv-processor:base
WORKDIR /app
VOLUME /app
ENTRYPOINT ["sh", "-c", "while true; do emscsv-processor; sleep 300; done"]
