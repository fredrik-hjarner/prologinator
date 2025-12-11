#!/usr/bin/env bun

import { Glob } from "bun";
import { $ } from "bun";
import { resolve } from "path";

// ==========================================================
// Configuration: Edit these patterns at the top of the file
// ==========================================================

// Array of glob patterns to include
export const includePatterns: string[] = [
    "prolog/**/*.pl",
];

// Array of glob patterns to exclude
export const excludePatterns: string[] = [
    "**/.git/**",
    "**/.cursor/**",
    "**/.husky/**",
    "**/.vscode/**",
    "**/internal_docs/**",
    "**/node_modules/**",
    "**/submodules/**",
    "**/*_test.pl",
    "**/*validation*.pl",
    "**/*constraint*.pl",
    "**/*macro*.pl",
    "**/*xod*.pl",
    "**/types/*.pl",
    "**/util/*.pl",
    "**/third_party/*.pl",
    "**/game.pl",
];

// ==========================================================
// File collection (filtered by patterns)
// ==========================================================

async function collectFilteredFiles(
    include: string[] = includePatterns,
    exclude: string[] = excludePatterns
): Promise<string[]> {
    const files: string[] = [];

    // Collect files matching any include pattern
    for (const pattern of include) {
        const glob = new Glob(pattern);
        for await (const file of glob.scan(".")) {
            files.push(file);
        }
    }

    // Remove duplicates and sort
    const uniqueFiles = [...new Set(files)].sort();

    // Filter out excluded patterns
    const filteredFiles: string[] = [];
    for (const file of uniqueFiles) {
        let shouldExclude = false;
        for (const excludePattern of exclude) {
            const excludeGlob = new Glob(excludePattern);
            if (excludeGlob.match(file)) {
                shouldExclude = true;
                break;
            }
        }
        if (!shouldExclude) {
            filteredFiles.push(file);
        }
    }

    return filteredFiles;
}

// ==========================================================
// Main execution
// ==========================================================

const files = await collectFilteredFiles();

if (files.length === 0) {
    console.error("Error: No .pl files found matching the patterns");
    process.exit(1);
}

console.log(`Found ${files.length} Prolog files`);

// Use absolute paths for the files
const absoluteFiles = files.map(f => resolve(process.cwd(), f));
const callgraphPath = resolve(process.cwd(), "scripts/call_graph/callgraph.pl");
const outputPath = resolve(process.cwd(), "graph.dot");

// Call swipl with the files
const proc = Bun.spawn(
    ["swipl", "-g", "main", callgraphPath, "--", ...absoluteFiles],
    {
        stdout: "pipe",
        stderr: "inherit",
    }
);

// Write output to graph.dot
const output = await new Response(proc.stdout).text();
await Bun.write(outputPath, output);

const exitCode = await proc.exited;

if (exitCode !== 0) {
    console.error(`Error: Process exited with code ${exitCode}`);
    process.exit(exitCode);
}

console.log(`Graph written to ${outputPath}`);

