#!/usr/bin/env bun

import { Glob } from "bun";
import { resolve, dirname, basename } from "path";
import { mkdir } from "fs/promises";

// ==========================================================
// Configuration: Edit these patterns at the top of the file
// ==========================================================

// Array of glob patterns to include
export const includePatterns: string[] = [
    "prolog/**/*.pl",
    "prolog/*.pl",
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
    "**/types/**/*",
    "**/util/*.pl",
    "**/third_party/*.pl",
    "**/game.pl",
    "**/perf.pl",
    "**/prolog/test_utils/**/*",
];

// ==========================================================
// File collection (filtered by patterns)
// ==========================================================

async function collectFilteredFiles(
    include: string[] = includePatterns,
    exclude: string[] = excludePatterns
): Promise<string[]> {
    const files: string[] = [];

    for (const pattern of include) {
        const glob = new Glob(pattern);
        for await (const file of glob.scan(".")) {
            files.push(file);
        }
    }

    const uniqueFiles = [...new Set(files)].sort();

    return uniqueFiles.filter(file => {
        for (const pattern of exclude) {
            const g = new Glob(pattern);
            if (g.match(file)) return false;
        }
        return true;
    });
}

// ==========================================================
// GPP preprocessing
// ==========================================================

async function preprocessWithGpp(files: string[]): Promise<string[]> {
    const tmpRoot = resolve(process.cwd(), ".gpp_tmp");
    await mkdir(tmpRoot, { recursive: true });

    const outFiles: string[] = [];

    for (const file of files) {
        const abs = resolve(process.cwd(), file);

        const outDir = resolve(tmpRoot, dirname(file));
        await mkdir(outDir, { recursive: true });

        const outFile = resolve(outDir, basename(file));

        const proc = Bun.spawn(
            ["gpp", "-P", "--warninglevel", "0", abs],
            { stdout: "pipe", stderr: "inherit" }
        );

        const output = await new Response(proc.stdout).text();
        const exitCode = await proc.exited;

        if (exitCode !== 0) {
            throw new Error(`gpp failed on ${file}`);
        }

        await Bun.write(outFile, output);
        outFiles.push(outFile);
    }

    return outFiles;
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

const preprocessedFiles = await preprocessWithGpp(files);

console.log(`Preprocessed ${preprocessedFiles.length} files with gpp`);

const callgraphPath = resolve(process.cwd(), "scripts/call_graph/callgraph.pl");
const outputPath = resolve(process.cwd(), "graph.dot");

// Call swipl with preprocessed files
const proc = Bun.spawn(
    ["swipl", "-g", "main", callgraphPath, "--", ...preprocessedFiles],
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
