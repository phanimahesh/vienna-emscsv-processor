FROM scratch
COPY root/ /
WORKDIR /app
VOLUME /app
ENTRYPOINT ["sh", "-c", "while true; do emscsv-processor; sleep 300; done"]
