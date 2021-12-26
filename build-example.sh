#!/usr/bin/env sh

set -eu

EXAMPLE=$1
TARGET=$(basename "$1" .lsp)
shift

echo "[INFO] Creating executable '$TARGET' for '$EXAMPLE'"

sbcl --noinform \
     --disable-debugger \
     --load "$EXAMPLE" \
     --eval "(sb-ext:save-lisp-and-die \"$TARGET\"
               :executable t
               :save-runtime-options t
               :toplevel '$TARGET:toplevel)"
