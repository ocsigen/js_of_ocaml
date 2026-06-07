#!/bin/bash
# Check that the EXTERNAL links in the manual are reachable.
#
# Internal references (modules, values, manual pages: {!...} / {{!...}...} /
# {{!page-...}...}) are now checked by odoc itself: `dune build @doc` fails when
# one does not resolve. odoc does NOT, however, check that external http(s) URLs
# are alive, so that is what this script covers — over the .mld manual.
#
# Usage: tools/check_manual_links.sh
set -u

MANUAL="$(cd "$(dirname "$0")/../manual" && pwd)"
errors=0

echo "Checking external links in $MANUAL ..."
echo

# odoc external links are {{:URL}text} or {:URL}. Pull out the URL (up to the
# closing brace or whitespace) and de-duplicate.
urls=$(grep -rhoE '\{\{?:https?://[^} 	]+' "$MANUAL" 2>/dev/null \
         | sed -E 's/^\{\{?://' | sort -u)

for url in $urls; do
  if curl -fsSL --head --max-time 15 -o /dev/null "$url" 2>/dev/null; then
    echo "✓ $url"
  else
    echo "✗ $url - UNREACHABLE"
    errors=$((errors + 1))
  fi
done

echo
if [ "$errors" -eq 0 ]; then
  echo "All external manual links are reachable."
  exit 0
else
  echo "$errors broken external link(s)."
  exit 1
fi
