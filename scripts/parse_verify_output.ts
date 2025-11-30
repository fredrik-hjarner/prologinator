#!/usr/bin/env bun

/**
 * Parses CiaoPP verification output and shows a clean summary
 */

const outputFile = process.env.VERIFY_OUTPUT_FILE || '/tmp/verify_strict_output.txt';
const content = Bun.file(outputFile);
const text = await content.text();

interface Warning {
  file: string;
  lines: string;
  assertion: string;
  reason: string;
}

const warnings: Warning[] = [];
const errors: string[] = [];

// Extract errors
const errorRegex = /ERROR[^:]*:.*/g;
const errorMatches = text.match(errorRegex);
if (errorMatches) {
  errors.push(...errorMatches);
}

// Extract warnings with context - track current file as we parse
let currentFile = 'unknown';
const lines = text.split('\n');

for (let i = 0; i < lines.length; i++) {
  const line = lines[i];
  
  // Track current file FIRST - can be on its own line or part of another line
  // The file line might be: "{In /path/to/file" or "{In /path/to/file}"
  const inFileMatch = line.match(/\{In ([^\}\n]+)/);
  if (inFileMatch) {
    currentFile = inFileMatch[1].trim();
  }
  
  // Look for warnings AFTER updating currentFile
  if (line.includes('could not verify assertion')) {
    // Extract line numbers
    const lineMatch = line.match(/\(lns ([0-9-]+)\)/);
    if (!lineMatch) continue;
    
    const warningLines = lineMatch[1];
    
    // Use currentFile if set, otherwise look backwards
    let fileForWarning = currentFile;
    if (fileForWarning === 'unknown') {
      for (let j = i - 1; j >= Math.max(0, i - 5); j--) {
        const fileMatch = lines[j].match(/\{In ([^\}\n]+)/);
        if (fileMatch) {
          fileForWarning = fileMatch[1].trim();
          break;
        }
      }
    }
    
    // Look ahead for the assertion (usually within next few lines)
    let assertion = 'unknown assertion';
    for (let j = i + 1; j < Math.min(i + 10, lines.length); j++) {
      const assertionMatch = lines[j].match(/:- check (calls|success|comp) ([^\n]+)/);
      if (assertionMatch) {
        assertion = `${assertionMatch[1]} ${assertionMatch[2].trim()}`;
        break;
      }
    }
    
    warnings.push({ 
      file: fileForWarning, 
      lines: warningLines, 
      assertion, 
      reason: '' 
    });
  }
}

// Deduplicate warnings (same file + lines + assertion)
const uniqueWarnings = new Map<string, Warning>();
for (const w of warnings) {
  const key = `${w.file}:${w.lines}:${w.assertion}`;
  if (!uniqueWarnings.has(key)) {
    uniqueWarnings.set(key, w);
  }
}

// Print summary
console.log('=== Verification Summary ===\n');

if (errors.length > 0) {
  console.log('❌ ERRORS found:');
  errors.slice(0, 10).forEach(e => console.log(`  ${e}`));
  console.log('\n❌ Verification found ERRORS');
  process.exit(1);
}

if (uniqueWarnings.size > 0) {
  console.log('⚠️  WARNINGS - Unverifiable assertions:\n');
  const sortedWarnings = Array.from(uniqueWarnings.values())
    .sort((a, b) => {
      if (a.file !== b.file) return a.file.localeCompare(b.file);
      return a.lines.localeCompare(b.lines);
    });
  
  for (const w of sortedWarnings.slice(0, 20)) {
    // Format as clickable path:line:column (most IDEs support this)
    const [startLine, endLine] = w.lines.split('-');
    // Make path relative to current directory
    const cwd = process.cwd() + '/';
    const relativePath = w.file.startsWith(cwd) 
      ? w.file.substring(cwd.length)
      : w.file;
    // Use end line if it's a range (more accurate - points to actual code, not predicate head)
    // Otherwise use the single line number
    const clickableLine = endLine ? endLine : startLine;
    const lineDisplay = endLine ? `${startLine}-${endLine}` : startLine;
    console.log(`  ${relativePath}:${clickableLine}:1: ${w.assertion} (range ${lineDisplay})`);
  }
  
  if (uniqueWarnings.size > 20) {
    console.log(`  ... and ${uniqueWarnings.size - 20} more`);
  }
  
  console.log('\n⚠️  Verification passed with warnings (see above)');
  console.log('   (Full details in ' + outputFile + ')');
} else {
  console.log('✅ Verification passed (no errors or warnings)');
}

process.exit(0);

