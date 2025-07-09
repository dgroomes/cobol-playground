#!/bin/bash
set -e

apt-get update

# Install packages needed for building GixSQL and running COBOL programs
# Key packages:
# - autoconf/automake/libtool: GNU build system (generates configure scripts)
# - bison/flex: Parser and lexer generators (GixSQL uses these to parse ESQL)
# - libspdlog-dev/libfmt-dev: C++ logging libraries that GixSQL depends on
# - gnucobol4: The COBOL compiler
# - libcob5: GnuCOBOL runtime library
apt-get install -y -q --no-install-recommends \
    build-essential autoconf automake libtool bison flex \
    pkg-config libspdlog-dev libfmt-dev git ca-certificates \
    gnucobol4 libcob5 libcob5-dev

update-ca-certificates

apt-get clean
rm -rf /var/lib/apt/lists/*
