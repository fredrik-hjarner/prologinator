#!/usr/bin/env bun

import { Glob } from "bun";

const MAX_LINES = process.env.MAX_LINES || "500";

// Find all .pl files under prolog folder
const files: string[] = [];
const glob = new Glob("prolog/**/*.pl");
for await (const file of glob.scan(".")) {
    files.push(file);
}

if (files.length === 0) {
    console.error("No .pl files found in prolog folder");
    process.exit(1);
}

files.sort();

console.log(`Found ${files.length} .pl file(s)`);

let hasErrors = false;

// Call max-line.ts for each file
for (const file of files) {
    const proc = Bun.spawn(
        ["bun", "scripts/max-line.ts", file],
        {
            env: { ...process.env, MAX_LINES },
            stdout: "inherit",
            stderr: "inherit",
        }
    );
    
    const exitCode = await proc.exited;
    
    if (exitCode !== 0) {
        hasErrors = true;
    }
}

if (hasErrors) {
    process.exit(1);
}

console.log("All line count checks succeeded");
process.exit(0);

