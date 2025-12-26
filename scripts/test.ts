#!/usr/bin/env bun

import { basename } from "path";
import { tmpdir } from "os";
import { join } from "path";
import { unlink } from "fs/promises";

let MODULE = process.argv[2];

if (!MODULE) {
    console.error("Error: MODULE argument is required");
    console.error("Usage: bun scripts/test.ts <module_path>");
    process.exit(1);
}

// Remove .pl extension if present
if (MODULE.endsWith(".pl")) {
    MODULE = MODULE.slice(0, -3);
}

const base = basename(MODULE);
const TIMEOUT_SECONDS = 10;

// Temp file for gpp output
const tmpFile = join(tmpdir(), `${base}.gpp.pl`);

// --------------------------------------------------
// Run gpp preprocessing
// --------------------------------------------------
const gpp = Bun.spawn(
    [
        "gpp",
        "-P",
        "--warninglevel",
        "0",
        "-I",
        ".",
        `${MODULE}.pl`,
        "-o",
        tmpFile
    ],
    {
        stdout: "pipe",
        stderr: "pipe"
    }
);

// Forward gpp stderr (warnings/errors)
for await (const chunk of gpp.stderr) {
    process.stderr.write(chunk);
}

const gppExitCode = await gpp.exited;

if (gppExitCode !== 0) {
    console.error("\n❌ Error: gpp preprocessing failed");
    process.exit(1);
}

// --------------------------------------------------
// Run Scryer-Prolog
// --------------------------------------------------
const prologCommand = `
use_module('submodules/scryer-prolog/src/tests/test_framework'),
use_module('${tmpFile}'),
main(${base}),
halt.
`;

const proc = Bun.spawn(
    ["scryer-prolog", "-g", prologCommand],
    {
        stdout: "pipe",
        stderr: "pipe"
    }
);

let output = "";

// Timeout guard
const timeoutId = setTimeout(() => {
    proc.kill();
    process.stdout.write(output);
    console.error(`\n❌ Error: Test timed out after ${TIMEOUT_SECONDS} seconds`);
    process.exit(1);
}, TIMEOUT_SECONDS * 1000);

// Capture stdout
for await (const chunk of proc.stdout) {
    const text = new TextDecoder().decode(chunk);
    output += text;
    process.stdout.write(chunk);
}

// Capture stderr
for await (const chunk of proc.stderr) {
    const text = new TextDecoder().decode(chunk);
    output += text;
    process.stderr.write(chunk);
}

const exitCode = await proc.exited;
clearTimeout(timeoutId);

// --------------------------------------------------
// Result checks
// --------------------------------------------------
if (output.includes("Failed")) {
    console.error(`\n❌ ERROR: test failed`);
    process.exit(1);
}

if (output.includes(" causes: ")) {
    console.error(`\n❌ ERROR: caused exception`);
    process.exit(1);
}

if (exitCode !== 0) {
    console.error(`\n❌ Test process exited with code ${exitCode}`);
    process.exit(1);
}

// --------------------------------------------------
// Cleanup
// --------------------------------------------------
try {
    await unlink(tmpFile);
} catch {
    // ignore cleanup errors
}

