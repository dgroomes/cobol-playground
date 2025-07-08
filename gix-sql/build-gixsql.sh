#!/bin/bash
set -e

BUILD_DIR=$(mktemp -d /tmp/gixsql-build.XXXXXX)
trap "rm -rf $BUILD_DIR" EXIT

cd "$BUILD_DIR"
git clone https://github.com/mridoni/gixsql.git
cd gixsql
git checkout v1.0.20b

./prepbuild.sh

# GixSQL's build system expects these variables to be defined in extra_files.mk
# Even if empty, they must exist or the Makefile.am will fail
# This is a quirk of their automake setup
echo -e "DOC_FILES =\nEXAMPLES_FILES =" > extra_files.mk

# autoreconf regenerates the GNU build system files
# -f: force regeneration even if files exist
# -v: verbose output
# -i: install missing auxiliary files
# This creates the 'configure' script from 'configure.ac'
autoreconf -fvi

./configure --disable-mysql --disable-odbc --disable-pgsql --disable-oracle

# Build the project. The '-j and nproc' part means "parallel + all available cores"
make -j$(nproc)

make install

# LLM: Update the dynamic linker's cache
# ldconfig maintains the runtime library cache (/etc/ld.so.cache)
# When we install new shared libraries (.so files) to /usr/local/lib,
# ldconfig ensures the system can find them at runtime
#
# Me: Is this really needed?
ldconfig
