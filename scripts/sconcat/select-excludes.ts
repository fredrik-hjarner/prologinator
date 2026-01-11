#!/usr/bin/env bun

import { excludePatterns } from "./feeder.js";
import {
    clearScreen, restoreTerminal
} from "./terminal.ts";

// ==========================================================
// Types
// ==========================================================

type PatternItem = { pattern: string; selected: boolean };

// ==========================================================
// Terminal utilities
// ==========================================================

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

function renderInterface(
    items: PatternItem[],
    currentIndex: number
) {
    clearScreen();
    
    const selectedCount = items.filter(item => item.selected).length;
    
    console.log('╔═══════════════════════════════════════════════════════════╗');
    console.log('║  Exclude Pattern Selection                                ║');
    console.log('╚═══════════════════════════════════════════════════════════╝\n');
    console.log(`Patterns: ${items.length} total, ${selectedCount} selected\n`);
    console.log('Controls:');
    console.log('  ↑/↓ - Navigate');
    console.log('  Space - Toggle selection');
    console.log('  a - Select all');
    console.log('  n - Unselect all');
    console.log('  Enter - Confirm and continue');
    console.log('  q - Quit\n');
    console.log('─'.repeat(60));

    for (let i = 0; i < items.length; i++) {
        const item = items[i]!;
        const isCurrent = i === currentIndex;
        
        const marker = item.selected ? '✓' : ' ';
        const cursor = isCurrent ? '▶' : ' ';
        const prefix = `${cursor} [${marker}]`;
        
        const line = `${prefix} ${item.pattern}`;
        if (isCurrent) {
            console.log(`\x1b[7m${line}\x1b[0m`);
        } else {
            console.log(line);
        }
    }

    console.log('─'.repeat(60));
}

async function interactiveSelectPatterns(patterns: string[]): Promise<string[]> {
    // Create initial state: all patterns selected (they're currently all active)
    let items: PatternItem[] = patterns.map(pattern => ({ pattern, selected: true }));
    let currentIndex = 0;

    while (true) {
        renderInterface(items, currentIndex);
        
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
            } else if (char === 'a') {
                items.forEach(item => item.selected = true);
            } else if (char === 'n') {
                items.forEach(item => item.selected = false);
            }
        }
    }

    // Return only selected patterns
    return items.filter(item => item.selected).map(item => item.pattern);
}

// ==========================================================
// Main: Interactive pattern selection
// ==========================================================

try {
    if (excludePatterns.length === 0) {
        // Output empty array if no patterns defined (so main.ts can parse it)
        process.stderr.write('[]\n');
        restoreTerminal();
        process.exit(0);
    }

    console.log(`Found ${excludePatterns.length} exclude patterns.\n`);
    console.log('Starting interactive selection...\n');
    await new Promise(resolve => setTimeout(resolve, 1000));

    const selectedPatterns = await interactiveSelectPatterns(excludePatterns);

    // Output selected patterns as JSON array to stderr (so it doesn't mix with interactive UI on stdout)
    // This allows the main script to read it
    // Write directly to stderr to avoid any console formatting
    process.stderr.write(JSON.stringify(selectedPatterns) + '\n');
    
    // Restore terminal after outputting JSON
    restoreTerminal();
} finally {
    restoreTerminal();
}



