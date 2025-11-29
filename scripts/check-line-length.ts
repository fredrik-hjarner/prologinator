#!/usr/bin/env bun

if (!process.env.MAX_LENGTH) {
    console.error(
        "Error: MAX_LENGTH environment variable is required"
    );
    process.exit(1);
}

const MAX_LENGTH = parseInt(process.env.MAX_LENGTH, 10);
const paths = process.argv.slice(2);

if (paths.length === 0) {
    process.exit(0);
}

let hasErrors = false;

for (const path of paths) {
    const text = await Bun.file(`prolog/${path}`).text();
    const lines = text.split("\n");
    
    for (let i = 0; i < lines.length; i++) {
        const line = lines[i];
        if (line && line.length > MAX_LENGTH) {
            console.error(
                `${path}:${i + 1}:` +
                ` line exceeds ${MAX_LENGTH} ` +
                `characters (${line.length})`
            );
            hasErrors = true;
        }
    }
}

process.exit(hasErrors ? 1 : 0);

