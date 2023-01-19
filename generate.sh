#!/usr/bin/env bash
mkdir -p "packages"
spago ls packages | while read package version location url ; do
  git clone --quiet --depth 1 --single-branch -b "$version" "${url:1:-2}" "packages/$package" &
done
wait
function modules () {
  local package="$1"
    grep \
      -r \
      --no-filename \
      --only-matching \
      --max-count 1 \
      --include '*.purs' \
      'module[[:space:]]*\([[:alpha:]]*\.\)*[[:alpha:]]*' \
      "packages/$package/src" | while read -r module name ; do
        echo "\"$name\""
      done | paste -s -d, -
}
function packages () {
  spago ls packages | while read -r package version location url ; do
    echo "\"$package\": [$(modules "$package")]"
  done | paste -s -d, -
}
echo "{$(packages)}" > database/latest.json
