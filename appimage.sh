#!/bin/sh

set -ex

rm -r AppDir_foom || true

stack build hsinstall --exec "hsinstall --mk-appimage foom"
