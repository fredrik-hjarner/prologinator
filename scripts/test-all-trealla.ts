#!/usr/bin/env bun

import { Glob } from "bun";
import { spawn } from "bun";

// ==========================================================
// Configuration: Edit these patterns at the top of the file
// ==========================================================

// Array of glob patterns to include (test files)
export const includePatterns: string[] = [
    "prolog/**/*_test.pl",
];

// Array of glob patterns to exclude
export const excludePatterns: string[] = [];

// ==========================================================
// File collection (filtered by patterns)
// ==========================================================

async function collectTestFiles(
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
// Test execution
// ==========================================================

async function runTest(testFile: string): Promise<boolean> {
    // Remove .pl extension for test script
    const modulePath = testFile.replace(/\.pl$/, "");
    
    console.log(`\n=== Testing ${testFile} ===`);
    
    const proc = spawn({
        cmd: ["./scripts/test_trealla.ts", modulePath],
        stdout: "inherit",
        stderr: "inherit"
    });

    const exitCode = await proc.exited;
    return exitCode === 0;
}

// ==========================================================
// Main
// ==========================================================

async function main() {
    const testFiles = await collectTestFiles();
    
    if (testFiles.length === 0) {
        console.error("No test files found");
        process.exit(1);
    }

    console.log(`Found ${testFiles.length} test file(s)`);

    for (const testFile of testFiles) {
        const success = await runTest(testFile);
        if (!success) {
            console.error(`\n❌ Test failed: ${testFile}`);
            process.exit(1);
        }
    }

    console.log(`\n✅ All ${testFiles.length} test(s) passed`);
}

main().catch((error) => {
    console.error("Error:", error);
    process.exit(1);
});

