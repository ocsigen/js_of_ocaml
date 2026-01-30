#!/bin/bash
# Check that all links and anchors in the manual are valid

set -e

MANUAL_DIR="$(dirname "$0")/../manual"
cd "$MANUAL_DIR"

errors=0

echo "Checking manual links and anchors..."
echo

# Check <<a_manual chapter="...">> references
echo "=== Chapter references ==="
for ref in $(grep -roh '<<a_manual chapter="[^"]*"' *.wiki 2>/dev/null | sed 's/<<a_manual chapter="//;s/"$//' | sort -u); do
  if [ -f "$ref.wiki" ]; then
    echo "✓ $ref"
  else
    echo "✗ $ref - MISSING"
    errors=$((errors + 1))
  fi
done
echo

# Check <<a_manual chapter="..." fragment="...">> references
echo "=== Fragment references ==="
for ref in $(grep -roh '<<a_manual chapter="[^"]*" fragment="[^"]*"' *.wiki 2>/dev/null | sed 's/<<a_manual chapter="//;s/" fragment="/:/;s/"$//' | sort -u); do
  file=$(echo "$ref" | cut -d: -f1)
  anchor=$(echo "$ref" | cut -d: -f2)
  if [ ! -f "$file.wiki" ]; then
    echo "✗ $ref - chapter file missing"
    errors=$((errors + 1))
  elif grep -q "@@id=\"$anchor\"@@" "$file.wiki"; then
    echo "✓ $ref"
  else
    echo "✗ $ref - anchor not found in $file.wiki"
    errors=$((errors + 1))
  fi
done
echo

# Check [[page|title]] wiki links
echo "=== Wiki links ==="
for page in $(grep -roh '\[\[[a-z_-]*|' *.wiki 2>/dev/null | sed 's/\[\[//;s/|$//' | sort -u); do
  if [ -f "$page.wiki" ]; then
    echo "✓ $page"
  else
    echo "✗ $page - MISSING"
    errors=$((errors + 1))
  fi
done
echo

# Check external links [[url|title]]
echo "=== External links ==="
for url in $(grep -roh '\[\[https\?://[^|]*|' *.wiki 2>/dev/null | sed 's/\[\[//;s/|$//' | sort -u); do
  # Use curl to check if URL is reachable (head request, follow redirects, timeout 10s)
  if curl --output /dev/null --silent --head --fail --max-time 10 -L "$url" 2>/dev/null; then
    echo "✓ $url"
  else
    echo "✗ $url - UNREACHABLE"
    errors=$((errors + 1))
  fi
done
echo

# Summary
echo "=== Summary ==="
if [ $errors -eq 0 ]; then
  echo "All links and anchors are valid."
  exit 0
else
  echo "$errors error(s) found."
  exit 1
fi
