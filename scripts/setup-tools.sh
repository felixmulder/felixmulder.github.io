#!/bin/sh
set -eu
cd "$(dirname "$0")/.."
. ./tools.env

case "$(uname -s)-$(uname -m)" in
    Darwin-arm64)
        target=aarch64-apple-darwin
        zola_sha=$ZOLA_DARWIN_SHA256
        lychee_sha=$LYCHEE_DARWIN_SHA256
        ;;
    Linux-x86_64)
        target=x86_64-unknown-linux-gnu
        zola_sha=$ZOLA_LINUX_SHA256
        lychee_sha=$LYCHEE_LINUX_SHA256
        ;;
    *) echo 'Supported platforms: macOS ARM64 and Linux x86-64.' >&2; exit 1 ;;
esac

temporary=$(mktemp -d)
trap 'rm -rf "$temporary"' EXIT
trap 'exit 130' INT TERM
mkdir -p .tools/bin

install_tool() {
    tool=$1 version=$2 url=$3 checksum=$4
    destination=".tools/bin/$tool"
    if [ -x "$destination" ] && [ "$("$destination" --version | head -n 1)" = "$tool $version" ]; then
        echo "Already installed: $tool $version"
        return
    fi
    directory="$temporary/$tool"
    mkdir -p "$directory"
    curl --fail --location --retry 3 "$url" --output "$directory/archive"
    if command -v sha256sum >/dev/null 2>&1; then
        printf '%s  %s\n' "$checksum" "$directory/archive" | sha256sum --check -
    else
        printf '%s  %s\n' "$checksum" "$directory/archive" | shasum -a 256 --check -
    fi
    tar -xzf "$directory/archive" -C "$directory"
    binary=$(find "$directory" -type f -name "$tool" -print)
    test -n "$binary"
    install -m 755 "$binary" "$destination"
    "$destination" --version
}

install_tool zola "$ZOLA_VERSION" \
    "https://github.com/getzola/zola/releases/download/v$ZOLA_VERSION/zola-v$ZOLA_VERSION-$target.tar.gz" "$zola_sha"
install_tool lychee "$LYCHEE_VERSION" \
    "https://github.com/lycheeverse/lychee/releases/download/lychee-v$LYCHEE_VERSION/lychee-$target.tar.gz" "$lychee_sha"
