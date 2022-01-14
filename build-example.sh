#!/bin/sh

set -eu

EXAMPLES="graph prediction"

# print msg, usage, available targets
target_error () {
    echo "[ERROR] $1"
    echo "[ERROR] Usage: $0 TARGET"
    echo "[ERROR]"
    echo "[ERROR] Available targets:"
    for e in $EXAMPLES; do
        echo "[ERROR] $e"
    done
}

if [ $# -lt 1 ]; then
    target_error "No target specified!"
    exit 1
fi

TARGET=$1

if [ -z "$(echo "$EXAMPLES" | grep -w "$TARGET")" ]; then
    target_error "Unknown target '$TARGET'!"
    exit 1
fi

echo "[INFO] Building '$TARGET'"
sbcl --noinform \
     --non-interactive \
     --eval "(push \"$(dirname $0)/\" asdf:*central-registry*)" \
     --eval "(asdf:make \"trie/$TARGET\")"
