#!/usr/bin/env bun

import { relative } from "path";
import collectFilteredFiles from "./feeder.js";
import concatenateFiles from "./concater.js";

// ==========================================================
// Configuration: Edit these at the top of the file
// ==========================================================

// Default output file (can be overridden by command line argument)
const DEFAULT_OUTPUT_FILE = "concat.xml";

// ==========================================================
// Types
// ==========================================================

type FileItem = { path: string; selected: boolean };

// ==========================================================
// Interactive file selection and reordering
// ==========================================================

// Global cleanup function to restore terminal state
function restoreTerminal() {
    try {
        process.stdin.setRawMode(false);
        process.stdin.pause();
        process.stdout.write('\x1b[?25h'); // Show cursor
    } catch (error) {
        // Ignore errors during cleanup
    }
}

// Set up cleanup handlers
process.on('SIGINT', () => {
    restoreTerminal();
    process.exit(130);
});

process.on('SIGTERM', () => {
    restoreTerminal();
    process.exit(143);
});

process.on('uncaughtException', (error) => {
    restoreTerminal();
    throw error;
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
            
            // Check for escape sequences (arrow keys)
            if (buffer.startsWith('\x1b[')) {
                // Wait a bit more for the complete sequence
                if (timeout) clearTimeout(timeout);
                timeout = setTimeout(() => {
                    cleanup();
                    resolve(buffer);
                }, 10);
            } else {
                // Regular character - resolve immediately
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

function renderInterface(
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

    // Display files
    for (let i = 0; i < items.length; i++) {
        const item = items[i]!;
        const relPath = relative(rootDir, item.path);
        const isCurrent = i === currentIndex;
        
        const marker = item.selected ? '✓' : ' ';
        const cursor = isCurrent ? '▶' : ' ';
        const prefix = `${cursor} [${marker}]`;
        
        // Display full path
        const displayPath = relPath;
        
        // Highlight current line
        const line = `${prefix} ${displayPath}`;
        if (isCurrent) {
            console.log(`\x1b[7m${line}\x1b[0m`); // Reverse video
        } else {
            console.log(line);
        }
    }

    console.log('─'.repeat(60));
}

async function interactiveSelect(files: string[]): Promise<string[]> {
    const rootDir = process.cwd();
    
    // Create initial state: all files selected, in original order
    let items: FileItem[] = files.map(path => ({ path, selected: true }));
    let currentIndex = 0;

    while (true) {
        renderInterface(items, currentIndex, rootDir);
        
        const input = await readKey();
        const firstChar = input[0] || '';
        
        // Handle input - check raw input first for special characters
        if (input === '\x03' || firstChar === '\x03') {
            // Ctrl+C - quit
            clearScreen();
            restoreTerminal();
            process.exit(0);
        } else if (input === '\r' || input === '\n' || firstChar === '\r' || firstChar === '\n') {
            // Enter - confirm
            break;
        } else if (firstChar === ' ' || input === ' ') {
            // Space - toggle selection
            items[currentIndex]!.selected = !items[currentIndex]!.selected;
        } else if (input.startsWith('\x1b[')) {
            // Arrow keys
            if (input === '\x1b[A') {
                // Up arrow
                currentIndex = Math.max(0, currentIndex - 1);
            } else if (input === '\x1b[B') {
                // Down arrow
                currentIndex = Math.min(items.length - 1, currentIndex + 1);
            }
        } else {
            // Regular character keys - check first character
            const char = firstChar.toLowerCase();
            
            if (char === 'q') {
                // 'q' - quit
                clearScreen();
                restoreTerminal();
                process.exit(0);
            } else if (char === 'w' && currentIndex > 0) {
                // 'w' - Move current item up
                [items[currentIndex], items[currentIndex - 1]] = [items[currentIndex - 1]!, items[currentIndex]!];
                currentIndex--;
            } else if (char === 's' && currentIndex < items.length - 1) {
                // 's' - Move current item down
                [items[currentIndex], items[currentIndex + 1]] = [items[currentIndex + 1]!, items[currentIndex]!];
                currentIndex++;
            } else if (char === 'a') {
                // 'a' - select all
                items.forEach(item => item.selected = true);
            } else if (char === 'n') {
                // 'n' - unselect all
                items.forEach(item => item.selected = false);
            }
        }
    }

    // Return only selected files in their current order
    return items.filter(item => item.selected).map(item => item.path);
}

// ==========================================================
// Main: Interactive selection then concatenate
// ==========================================================

try {
    const filteredFiles = await collectFilteredFiles();

    if (filteredFiles.length === 0) {
        console.log('No files matched the include/exclude patterns.');
        process.exit(0);
    }

    console.log(`Found ${filteredFiles.length} files matching patterns.\n`);
    console.log('Starting interactive selection...\n');
    await new Promise(resolve => setTimeout(resolve, 1000));

    const selectedFiles = await interactiveSelect(filteredFiles);

    if (selectedFiles.length === 0) {
        clearScreen();
        console.log('\nNo files selected. Exiting.');
        process.exit(0);
    }

    // Clear screen and write concatenated files
    clearScreen();

    // Get output filename from command line argument or use default
    const outputFile = process.argv[2] || DEFAULT_OUTPUT_FILE;
    console.log(`Concatenating ${selectedFiles.length} files to ${outputFile}...\n`);

    // Generate concatenated output
    const output = await concatenateFiles(selectedFiles);

    // Write to file
    await Bun.write(outputFile, output);
    console.log(`✓ Successfully wrote ${selectedFiles.length} files to ${outputFile}`);
} finally {
    // Ensure terminal is restored even if an error occurs
    restoreTerminal();
}

