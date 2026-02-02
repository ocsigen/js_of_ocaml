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

# Check <<a_api subproject="...">> references
echo "=== API references ==="
API_DOC_DIR="../_wikidoc/doc/dev/api"

if [ ! -d "$API_DOC_DIR" ]; then
  echo "Warning: API documentation not found at $API_DOC_DIR"
  echo "Run 'make doc' to generate API documentation first."
  echo
else
  grep -roh '<<a_api[^>]*>>' *.wiki 2>/dev/null | sort -u | while IFS= read -r ref; do
    # Extract subproject="..."
    subproject=$(echo "$ref" | sed -n 's/.*subproject="\([^"]*\)".*/\1/p')
    # Extract content after | and before >>
    content=$(echo "$ref" | sed -n 's/.*|\([^>]*\)>>/\1/p')
    # Extract type (val, module, type, exception, index)
    item_type=$(echo "$content" | awk '{print $1}')
    # Extract module path
    module_path=$(echo "$content" | awk '{print $2}')

    # Check subproject exists
    if [ ! -d "$API_DOC_DIR/$subproject" ]; then
      echo "✗ $subproject|$content - subproject not found"
      echo "API_ERROR" >> /tmp/check_manual_errors.$$
      continue
    fi

    # Handle index links
    if [ "$item_type" = "index" ]; then
      if [ -f "$API_DOC_DIR/$subproject/index.html" ]; then
        echo "✓ $subproject|index"
      else
        echo "✗ $subproject|index - index.html not found"
        echo "API_ERROR" >> /tmp/check_manual_errors.$$
      fi
      continue
    fi

    # Convert Js_of_ocaml.Js.string -> Js_of_ocaml/Js for val/type/exception
    # Convert Js_of_ocaml.Js.Opt -> Js_of_ocaml/Js/Opt for module
    if [ "$item_type" = "module" ]; then
      # For modules, full path is the directory
      dir_path=$(echo "$module_path" | tr '.' '/')
      if [ -d "$API_DOC_DIR/$subproject/$dir_path" ]; then
        echo "✓ $subproject|$content"
      else
        echo "✗ $subproject|$content - module not found"
        echo "API_ERROR" >> /tmp/check_manual_errors.$$
      fi
    else
      # For val/type/exception, check parent module's index.html
      # Remove last component (the item name)
      parent_path=$(echo "$module_path" | sed 's/\.[^.]*$//' | tr '.' '/')
      if [ -f "$API_DOC_DIR/$subproject/$parent_path/index.html" ]; then
        echo "✓ $subproject|$content"
      else
        echo "✗ $subproject|$content - module not found"
        echo "API_ERROR" >> /tmp/check_manual_errors.$$
      fi
    fi
  done
  # Count errors from subshell
  if [ -f /tmp/check_manual_errors.$$ ]; then
    api_errors=$(wc -l < /tmp/check_manual_errors.$$)
    errors=$((errors + api_errors))
    rm -f /tmp/check_manual_errors.$$
  fi
  echo
fi

# Summary
echo "=== Summary ==="
if [ $errors -eq 0 ]; then
  echo "All links and anchors are valid."
  exit 0
else
  echo "$errors error(s) found."
  exit 1
fi
