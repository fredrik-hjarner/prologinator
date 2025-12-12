#!/usr/bin/env bun

import { spawn } from "child_process";
import { relative } from "path";
import collectFilteredFiles from "./feeder.js";
import concatenateFiles from "./concater.js";

const DEFAULT_OUTPUT_FILE = "concat.xml";

// ==========================================================
// Types
// ==========================================================

type FileItem = { path: string; selected: boolean };

// ==========================================================
// Terminal utilities
// ==========================================================

function restoreTerminal() {
    try {
        process.stdin.setRawMode(false);
        process.stdin.pause();
        process.stdout.write('\x1b[?25h');
    } catch (error) {
        // Ignore errors during cleanup
    }
}

process.on('SIGINT', () => {
    restoreTerminal();
    process.exit(130);
});

process.on('SIGTERM', () => {
    restoreTerminal();
    process.exit(143);
});

async function readKey(): Promise<string> {
    return new Promise((resolve) => {
        const stdin = process.stdin;
        stdin.setRawMode(true);
        stdin.resume();
        stdin.setEncoding('utf8');

        let buffer = '';
        let timeout: ReturnType<typeof setTimeout> | null = null;
        
        const cleanup = () => {
            if (timeout) clearTimeout(timeout);
            stdin.removeListener('data', handler);
            stdin.setRawMode(false);
            stdin.pause();
        };
        
        const handler = (data: Buffer) => {
            buffer += data.toString();
            
            if (buffer.startsWith('\x1b[')) {
                if (timeout) clearTimeout(timeout);
                timeout = setTimeout(() => {
                    cleanup();
                    resolve(buffer);
                }, 10);
            } else {
                cleanup();
                resolve(buffer);
            }
        };

        stdin.on('data', handler);
    });
}

function clearScreen() {
    process.stdout.write('\x1b[2J\x1b[H');
}

// ==========================================================
// Run exclude selector and get selected patterns
// ==========================================================

async function runExcludeSelector(): Promise<string[]> {
    return new Promise((resolve, reject) => {
        const child = spawn("bun", ["scripts/sconcat/select-excludes.ts"], {
            stdio: ["inherit", "inherit", "pipe"]
        });

        let output = "";
        
        child.stderr.on("data", (data) => {
            output += data.toString();
        });

        child.on("close", (code) => {
            if (code !== 0) {
                reject(new Error(`Exclude selector exited with code ${code}`));
                return;
            }
            
            try {
                // Parse the JSON output - look for a line that starts with '[' (JSON array)
                const lines = output.trim().split('\n').map(line => line.trim()).filter(line => line.length > 0);
                // Find the line that looks like JSON (starts with '[')
                const jsonLine = lines.find(line => line.startsWith('['));
                if (!jsonLine) {
                    reject(new Error(`No JSON output from exclude selector. Output was: ${JSON.stringify(output)}`));
                    return;
                }
                const selectedPatterns = JSON.parse(jsonLine);
                resolve(selectedPatterns);
            } catch (error) {
                reject(new Error(`Failed to parse exclude selector output: ${error}. Output was: ${JSON.stringify(output)}`));
            }
        });

        child.on("error", (error) => {
            reject(error);
        });
    });
}

// ==========================================================
// Interactive file selection and reordering
// ==========================================================

function renderFileInterface(
    items: FileItem[],
    currentIndex: number,
    rootDir: string
) {
    clearScreen();
    
    const selectedCount = items.filter(item => item.selected).length;
    
    console.log('╔═══════════════════════════════════════════════════════════╗');
    console.log('║  File Selection & Reordering                              ║');
    console.log('╚═══════════════════════════════════════════════════════════╝\n');
    console.log(`Files: ${items.length} total, ${selectedCount} selected\n`);
    console.log('Controls:');
    console.log('  ↑/↓ - Navigate');
    console.log('  Space - Toggle selection');
    console.log('  a - Select all');
    console.log('  n - Unselect all');
    console.log('  w - Move item up');
    console.log('  s - Move item down');
    console.log('  Enter - Confirm and generate output');
    console.log('  q - Quit\n');
    console.log('─'.repeat(60));

    for (let i = 0; i < items.length; i++) {
        const item = items[i]!;
        const relPath = relative(rootDir, item.path);
        const isCurrent = i === currentIndex;
        
        const marker = item.selected ? '✓' : ' ';
        const cursor = isCurrent ? '▶' : ' ';
        const prefix = `${cursor} [${marker}]`;
        
        const displayPath = relPath;
        const line = `${prefix} ${displayPath}`;
        if (isCurrent) {
            console.log(`\x1b[7m${line}\x1b[0m`);
        } else {
            console.log(line);
        }
    }

    console.log('─'.repeat(60));
}

async function interactiveSelectFiles(files: string[]): Promise<string[]> {
    const rootDir = process.cwd();
    
    let items: FileItem[] = files.map(path => ({ path, selected: true }));
    let currentIndex = 0;

    while (true) {
        renderFileInterface(items, currentIndex, rootDir);
        
        const input = await readKey();
        const firstChar = input[0] || '';
        
        if (input === '\x03' || firstChar === '\x03') {
            clearScreen();
            restoreTerminal();
            process.exit(0);
        } else if (input === '\r' || input === '\n' || firstChar === '\r' || firstChar === '\n') {
            break;
        } else if (firstChar === ' ' || input === ' ') {
            items[currentIndex]!.selected = !items[currentIndex]!.selected;
        } else if (input.startsWith('\x1b[')) {
            if (input === '\x1b[A') {
                currentIndex = Math.max(0, currentIndex - 1);
            } else if (input === '\x1b[B') {
                currentIndex = Math.min(items.length - 1, currentIndex + 1);
            }
        } else {
            const char = firstChar.toLowerCase();
            
            if (char === 'q') {
                clearScreen();
                restoreTerminal();
                process.exit(0);
            } else if (char === 'w' && currentIndex > 0) {
                [items[currentIndex], items[currentIndex - 1]] = [items[currentIndex - 1]!, items[currentIndex]!];
                currentIndex--;
            } else if (char === 's' && currentIndex < items.length - 1) {
                [items[currentIndex], items[currentIndex + 1]] = [items[currentIndex + 1]!, items[currentIndex]!];
                currentIndex++;
            } else if (char === 'a') {
                items.forEach(item => item.selected = true);
            } else if (char === 'n') {
                items.forEach(item => item.selected = false);
            }
        }
    }

    return items.filter(item => item.selected).map(item => item.path);
}

// ==========================================================
// Main: Two-stage selection then concatenate
// ==========================================================

try {
    // Stage 1: Select exclude patterns
    console.log('Stage 1: Selecting exclude patterns...\n');
    const selectedExcludePatterns = await runExcludeSelector();
    
    clearScreen();
    console.log(`Selected ${selectedExcludePatterns.length} exclude patterns.\n`);
    await new Promise(resolve => setTimeout(resolve, 1000));

    // Stage 2: Collect files with selected exclude patterns
    const filteredFiles = await collectFilteredFiles(undefined, selectedExcludePatterns);

    if (filteredFiles.length === 0) {
        console.log('No files matched the include/exclude patterns.');
        process.exit(0);
    }

    console.log(`Found ${filteredFiles.length} files matching patterns.\n`);
    console.log('Starting interactive file selection...\n');
    await new Promise(resolve => setTimeout(resolve, 1000));

    // Stage 3: Select files
    const selectedFiles = await interactiveSelectFiles(filteredFiles);

    if (selectedFiles.length === 0) {
        clearScreen();
        console.log('\nNo files selected. Exiting.');
        process.exit(0);
    }

    // Stage 4: Concatenate
    clearScreen();
    const outputFile = process.argv[2] || DEFAULT_OUTPUT_FILE;
    console.log(`Concatenating ${selectedFiles.length} files to ${outputFile}...\n`);

    const output = await concatenateFiles(selectedFiles);
    await Bun.write(outputFile, output);
    console.log(`✓ Successfully wrote ${selectedFiles.length} files to ${outputFile}`);
} finally {
    restoreTerminal();
}



