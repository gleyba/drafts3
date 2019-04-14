#! /usr/bin/env bash

set -eu
shopt -s nullglob

pushd $1
cargo build --release
popd
cp $1/target/release/$2 $3
