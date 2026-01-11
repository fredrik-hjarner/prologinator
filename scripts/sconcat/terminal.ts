export const ANSI = {
    CLEAR: '\x1b[2J\x1b[H',     // Clear screen + Move home
    HIDE_CURSOR: '\x1b[?25l',   // Hide cursor
    SHOW_CURSOR: '\x1b[?25h',   // Show cursor
    REVERSE: '\x1b[7m',         // Swap bg/foreground colors
    RESET: '\x1b[0m',           // Reset formatting
    Up: '\x1b[A',
    Down: '\x1b[B',
};

/**
 * Puts terminal in raw mode to read single keystrokes.
 */
export async function waitForKey(): Promise<string> {
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
            
            // If it starts with ESC (\x1b), it might be a sequence (like arrow keys)
            // We wait 10ms to see if more bytes arrive.
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

export function clearScreen() {
    process.stdout.write(ANSI.CLEAR);
}

export function restoreTerminal() {
    try {
        process.stdin.setRawMode(false);
        process.stdin.pause();
        process.stdout.write(ANSI.SHOW_CURSOR);
    } catch (e) { /* ignore */ }
}

// Global safety net
process.on('SIGINT', () => { 
    restoreTerminal(); process.exit(0);
});
process.on('SIGTERM', () => {
    restoreTerminal(); process.exit(0);
});
process.on('exit', () => restoreTerminal());
process.on('uncaughtException', (error) => {
    restoreTerminal();
    throw error;
});