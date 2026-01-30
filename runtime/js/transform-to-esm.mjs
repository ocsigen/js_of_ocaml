#!/usr/bin/env node
// Transform runtime/js files to ES modules
import { readFileSync, writeFileSync, readdirSync } from 'fs';
import { join, basename } from 'path';

const runtimeDir = '/home/hugo/js_of_ocaml/runtime/js';

// Phase 1: Build provider map
// Maps: providedName -> { file, hasConditions, variants: [{conditions, lineNum}] }
const providerMap = new Map();

// Get all JS files
const jsFiles = readdirSync(runtimeDir)
  .filter(f => f.endsWith('.js') && f !== 'index.js' && f !== 'transform-to-esm.mjs')
  .sort();

console.log(`Found ${jsFiles.length} JS files to process`);

// Parse a provides block to extract name and conditions
function parseProvides(lines, startIdx) {
  const providesLine = lines[startIdx];
  const match = providesLine.match(/^\/\/Provides:\s*(\S+)/);
  if (!match) return null;

  const name = match[1];
  const conditions = [];

  // Look for conditions (//If:, //Version:) following the provides
  let i = startIdx + 1;
  while (i < lines.length) {
    const line = lines[i];
    if (line.startsWith('//If:')) {
      const cond = line.replace('//If:', '').trim();
      conditions.push({ type: 'if', value: cond });
    } else if (line.startsWith('//Version:')) {
      const cond = line.replace('//Version:', '').trim();
      conditions.push({ type: 'version', value: cond });
    } else if (line.startsWith('//Requires:') || line.startsWith('//Alias:') || line.startsWith('//Weakdef')) {
      // These are metadata, continue
    } else {
      break;
    }
    i++;
  }

  return { name, conditions, endMetaLine: i };
}

// Generate export name - keep original name even for conditional variants
// The //If: and //Version: annotations are preserved as comments for the compiler
function generateExportName(baseName, conditions) {
  // Always return the base name - no suffix for conditional variants
  return baseName;
}

// Extract declaration name from a line
function extractDeclName(line) {
  // Check for function declaration
  const funcMatch = line.match(/^function\s+(\w+)\s*\(/);
  if (funcMatch) return { type: 'function', name: funcMatch[1] };

  // Check for class declaration
  const classMatch = line.match(/^class\s+(\w+)/);
  if (classMatch) return { type: 'class', name: classMatch[1] };

  // Check for var declaration with =
  const varMatch = line.match(/^var\s+(\w+)\s*=/);
  if (varMatch) return { type: 'var', name: varMatch[1] };

  // Check for var declaration without = on same line
  const varDeclMatch = line.match(/^var\s+(\w+)\s*$/);
  if (varDeclMatch) return { type: 'var', name: varDeclMatch[1] };

  return null;
}

// Check if a line is a non-metadata comment (regular comment)
function isRegularComment(line) {
  if (!line.startsWith('//')) return false;
  // Check if it's a metadata comment
  if (line.startsWith('//Provides:') ||
      line.startsWith('//Requires:') ||
      line.startsWith('//If:') ||
      line.startsWith('//Version:') ||
      line.startsWith('//Alias:') ||
      line.startsWith('//Weakdef')) {
    return false;
  }
  return true;
}

// First pass: scan all files to build provider map
for (const file of jsFiles) {
  const filePath = join(runtimeDir, file);
  const content = readFileSync(filePath, 'utf-8');
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    if (lines[i].startsWith('//Provides:')) {
      const parsed = parseProvides(lines, i);
      if (parsed) {
        const { name, conditions } = parsed;

        if (!providerMap.has(name)) {
          providerMap.set(name, {
            file,
            hasConditions: conditions.length > 0,
            variants: []
          });
        }

        const entry = providerMap.get(name);
        entry.variants.push({ conditions, lineNum: i });

        // If this file differs from recorded, or new conditions found, mark as having conditions
        if (entry.file !== file || conditions.length > 0) {
          entry.hasConditions = true;
        }
      }
    }
  }
}

console.log(`Built provider map with ${providerMap.size} entries`);

// Check for providers with multiple variants
let multiVariantCount = 0;
for (const [name, info] of providerMap) {
  if (info.variants.length > 1) {
    multiVariantCount++;
    console.log(`  Multi-variant: ${name} (${info.variants.length} variants in ${info.file})`);
  }
}
console.log(`Found ${multiVariantCount} multi-variant providers`);

// Phase 2: Transform each file
for (const file of jsFiles) {
  const filePath = join(runtimeDir, file);
  const content = readFileSync(filePath, 'utf-8');
  const lines = content.split('\n');

  // Collect all requires for this file
  const requires = new Set();
  // Collect provides in this file (to skip self-requires)
  const providesInFile = new Set();

  // First pass: identify all provides and requires
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    if (line.startsWith('//Provides:')) {
      const parsed = parseProvides(lines, i);
      if (parsed) {
        providesInFile.add(parsed.name);
      }
    }

    if (line.startsWith('//Requires:')) {
      const reqMatch = line.match(/^\/\/Requires:\s*(.+)$/);
      if (reqMatch) {
        const deps = reqMatch[1].split(',').map(d => d.trim());
        for (const dep of deps) {
          requires.add(dep);
        }
      }
    }
  }

  // Remove self-requires
  for (const name of providesInFile) {
    requires.delete(name);
  }

  // Build import statements grouped by source file
  const importsByFile = new Map();
  for (const req of requires) {
    const provider = providerMap.get(req);
    if (!provider) {
      console.warn(`Warning: ${file} requires unknown '${req}'`);
      continue;
    }
    const sourceFile = provider.file;
    if (sourceFile === file) continue; // Same file, skip

    if (!importsByFile.has(sourceFile)) {
      importsByFile.set(sourceFile, []);
    }
    importsByFile.get(sourceFile).push(req);
  }

  // Generate import statements
  const imports = [];
  for (const [sourceFile, deps] of Array.from(importsByFile).sort((a, b) => a[0].localeCompare(b[0]))) {
    const sortedDeps = deps.sort();
    imports.push(`import { ${sortedDeps.join(', ')} } from './${sourceFile}';`);
  }

  // Find insert position for imports:
  // - After license header (block of // comments at the start)
  // - Before the first //Provides: block
  let insertPos = 0;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // We're done with header when we see first //Provides:
    if (line.startsWith('//Provides:')) {
      insertPos = i;
      break;
    }

    // Continue past license header (// comments and blank lines)
    if (line.startsWith('//') || line.trim() === '') {
      insertPos = i + 1;
    }
  }

  // Second pass: transform the file
  const newLines = [];

  // Add lines before insert position
  for (let i = 0; i < insertPos; i++) {
    newLines.push(lines[i]);
  }

  // Add imports
  if (imports.length > 0) {
    newLines.push(...imports);
    newLines.push('');
  }

  // Track which lines we've already processed (for finding function declarations)
  const processedLines = new Set();

  // Process remaining lines
  let i = insertPos;
  while (i < lines.length) {
    const line = lines[i];

    // Skip if already processed
    if (processedLines.has(i)) {
      i++;
      continue;
    }

    // Handle //Provides: block
    if (line.startsWith('//Provides:')) {
      const parsed = parseProvides(lines, i);
      if (parsed) {
        const { name: providedName, conditions, endMetaLine } = parsed;
        const exportName = generateExportName(providedName, conditions);
        const needsRename = exportName !== providedName;

        // Copy the //Provides: line as-is (keeping metadata)
        newLines.push(line);
        i++;

        // Copy all metadata lines (//If:, //Version:, //Alias:, //Weakdef)
        // But delete //Requires: lines
        while (i < lines.length) {
          const metaLine = lines[i];
          if (metaLine.startsWith('//If:') ||
              metaLine.startsWith('//Version:') ||
              metaLine.startsWith('//Alias:') ||
              metaLine.startsWith('//Weakdef')) {
            newLines.push(metaLine);
            i++;
          } else if (metaLine.startsWith('//Requires:')) {
            // Skip/delete requires line
            i++;
          } else {
            break;
          }
        }

        // Skip any regular comment lines (non-metadata comments between metadata and declaration)
        while (i < lines.length && isRegularComment(lines[i])) {
          newLines.push(lines[i]);
          i++;
        }

        // Now find the actual declaration that matches the provided name
        // It might be on the immediate next line or we might need to look ahead
        if (i < lines.length) {
          const declLine = lines[i];
          const declInfo = extractDeclName(declLine);

          // Check if this declaration matches the provided name
          if (declInfo && declInfo.name === providedName) {
            // Direct match - apply export
            if (needsRename) {
              if (declInfo.type === 'function') {
                newLines.push(declLine.replace(`function ${providedName}`, `export function ${exportName}`));
              } else if (declInfo.type === 'class') {
                newLines.push(declLine.replace(`class ${providedName}`, `export class ${exportName}`));
              } else {
                newLines.push(declLine.replace(`var ${providedName}`, `export let ${exportName}`));
              }
            } else {
              // Replace var with let for exports
              const exportLine = declInfo.type === 'var'
                ? declLine.replace(/^var\s+/, 'let ')
                : declLine;
              newLines.push(`export ${exportLine}`);
            }
            processedLines.add(i);
            i++;
          } else if (declInfo && declInfo.name !== providedName) {
            // The declaration doesn't match - it might be a helper variable
            // Output as-is and look for the matching function
            newLines.push(declLine);
            processedLines.add(i);
            i++;

            // Search forward for the matching function
            while (i < lines.length && !lines[i].startsWith('//Provides:')) {
              const searchLine = lines[i];
              const searchDeclInfo = extractDeclName(searchLine);

              if (searchDeclInfo && searchDeclInfo.name === providedName) {
                // Found it!
                if (needsRename) {
                  if (searchDeclInfo.type === 'function') {
                    newLines.push(searchLine.replace(`function ${providedName}`, `export function ${exportName}`));
                  } else if (searchDeclInfo.type === 'class') {
                    newLines.push(searchLine.replace(`class ${providedName}`, `export class ${exportName}`));
                  } else {
                    newLines.push(searchLine.replace(`var ${providedName}`, `export let ${exportName}`));
                  }
                } else {
                  // Replace var with let for exports
                  const exportLine = searchDeclInfo.type === 'var'
                    ? searchLine.replace(/^var\s+/, 'let ')
                    : searchLine;
                  newLines.push(`export ${exportLine}`);
                }
                processedLines.add(i);
                i++;
                break;
              } else {
                // Not the matching declaration, copy as-is
                newLines.push(searchLine);
                processedLines.add(i);
                i++;
              }
            }
          } else {
            // No declaration info - just copy the line
            newLines.push(declLine);
            i++;
          }
        }
        continue;
      }
    }

    // Skip standalone //Requires: lines (should not exist outside of //Provides blocks, but just in case)
    if (line.startsWith('//Requires:')) {
      i++;
      continue;
    }

    newLines.push(line);
    i++;
  }

  // Write transformed file
  const newContent = newLines.join('\n');
  writeFileSync(filePath, newContent);
  console.log(`Transformed: ${file}`);
}

// Phase 3: Create index.js
const indexLines = ['// Auto-generated index file for js_of_ocaml runtime'];
for (const file of jsFiles) {
  indexLines.push(`export * from './${file}';`);
}
indexLines.push('');

writeFileSync(join(runtimeDir, 'index.js'), indexLines.join('\n'));
console.log('Created index.js');

console.log('Done!');
