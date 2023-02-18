#!/usr/bin/env bash


function spago-packages () {
  spago ls packages --config "spago-acme.dhall"
}

function packages () {
  spago bundle-app -p node -t index.mjs
  spago-packages | while read -r package version location url ; do
    if [ -d "packages/$package/src" ]; then
      echo "\"$package\": $(node index.mjs "packages/$package/src")"
    fi
  done | paste -s -d, -
}

mkdir -p "packages"
spago-packages | while read package version location url ; do
  git clone --quiet --depth 1 --single-branch -b "$version" "${url:1:-2}" "packages/$package" &
done
wait

echo "{$(packages)}" > docs/latest.json
