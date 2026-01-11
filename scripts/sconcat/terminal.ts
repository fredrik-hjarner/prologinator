export const ANSI = {
    CLEAR: '\x1b[2J\x1b[H',     // Clear screen + Move home
    HIDE_CURSOR: '\x1b[?25l',   // Hide cursor
    SHOW_CURSOR: '\x1b[?25h',   // Show cursor
    REVERSE: '\x1b[7m',         // Swap bg/foreground colors
    RESET: '\x1b[0m',           // Reset formatting
    Up: '\x1b[A',
    Down: '\x1b[B',
};

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