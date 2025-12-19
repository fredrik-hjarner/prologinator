#!/usr/bin/env bun

const MAX_LINES = parseInt(
    process.env.MAX_LINES || "500",
    10
);
const paths = process.argv.slice(2);

if (paths.length === 0) {
    process.exit(0);
}

let hasErrors = false;

for (const path of paths) {
    const text = await Bun.file(`${path}`).text();
    const lines = text.split("\n");
    const lineCount = lines.length;
    
    if (lineCount > MAX_LINES) {
        console.error(
            `${path}: file exceeds ${MAX_LINES} ` +
            `lines (${lineCount})`
        );
        hasErrors = true;
    }
}

process.exit(hasErrors ? 1 : 0);

