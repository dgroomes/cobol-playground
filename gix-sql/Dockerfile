FROM debian:12-slim

WORKDIR /app

ENV DEBIAN_FRONTEND=noninteractive
ENV LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

# Copy dependency installation script first (rarely changes)
COPY install-dependencies.sh .
RUN chmod +x install-dependencies.sh && ./install-dependencies.sh

# Copy build script (changes less frequently)
COPY build-gixsql.sh .
RUN chmod +x build-gixsql.sh && ./build-gixsql.sh

# Copy runtime files (changes most frequently)
COPY run-example.sh hello-world.cbl ./
RUN chmod +x run-example.sh

CMD ["./run-example.sh"]