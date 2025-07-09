FROM debian:12-slim

WORKDIR /app

ENV DEBIAN_FRONTEND=noninteractive
ENV LD_LIBRARY_PATH=/usr/local/lib

# Copy dependency installation script first (rarely changes)
COPY 1-install-dependencies.sh .
RUN chmod +x 1-install-dependencies.sh && ./1-install-dependencies.sh

# Copy build script (changes less frequently)
COPY 2-build-gixsql.sh .
RUN chmod +x 2-build-gixsql.sh && ./2-build-gixsql.sh

# Copy runtime files (changes most frequently)
COPY 3-run-example.sh hello-world.cbl ./
RUN chmod +x 3-run-example.sh

CMD ["./3-run-example.sh"]