#!/usr/bin/env bun

import { Glob } from "bun";

const MAX_LENGTH = process.env.MAX_LENGTH || "60";

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

// Call max-len.ts for each file
for (const file of files) {
    const proc = Bun.spawn(
        ["bun", "scripts/max-len.ts", file],
        {
            env: { ...process.env, MAX_LENGTH },
            stdout: "inherit",
            stderr: "inherit",
        }
    );
    
    const exitCode = await proc.exited;
    
    if (exitCode !== 0) {
        process.exit(1);
    }
}

console.log("All line length checks succeeded");
process.exit(0);

