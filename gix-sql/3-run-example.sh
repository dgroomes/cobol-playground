#!/bin/bash
set -e

rm -f hello-world-preprocessed.cbl hello-world hello.db

# Preprocess: Convert EXEC SQL statements to COBOL CALLs
gixsql hello-world.cbl hello-world-preprocessed.cbl -S -I/usr/local/share/gixsql/copy

# Compile: Create executable linking against GixSQL runtime
cobc -x hello-world-preprocessed.cbl -L/usr/local/lib -lgixsql -I/usr/local/share/gixsql/copy -o hello-world

# Environment variables needed at runtime:
# LD_LIBRARY_PATH: Where to find shared libraries (.so files)
# COB_LIBRARY_PATH: Where GnuCOBOL looks for COBOL modules
# GIXSQL_DEFAULT_DRIVER: Which database driver to use (SQLite, MySQL, etc.)
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
export COB_LIBRARY_PATH=/usr/local/lib:$COB_LIBRARY_PATH
export GIXSQL_DEFAULT_DRIVER=SQLite

./hello-world

[ -f hello.db ] || exit 1