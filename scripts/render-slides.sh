#!/bin/sh
set -eu
source=$1
output=$2
pandoc=$3

# Read only the explicit slide_format field in the TOML header.
format=$(awk '
    $0 == "+++" { if (++delimiters == 2) exit; next }
    delimiters == 1 && $1 == "slide_format" { gsub(/"/, "", $3); print $3 }
' "$source")

mkdir -p "$(dirname "$output")"
temporary=$(mktemp -d)
trap 'rm -rf "$temporary"' EXIT
trap 'exit 130' INT TERM
awk '$0 == "+++" && delimiters < 2 { delimiters++; next } delimiters == 2 { print }' \
    "$source" > "$temporary/body"
case "$format" in
    html) cp "$temporary/body" "$output" ;;
    markdown) "$pandoc" --from=markdown --to=revealjs --syntax-highlighting=none \
        "$temporary/body" --output="$output" ;;
    *) echo "Missing or unsupported slide_format in $source" >&2; exit 1 ;;
esac
