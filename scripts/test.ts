#!/usr/bin/env bun

import { basename } from "path";

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

const prologCommand = `use_module('submodules/scryer-prolog/src/tests/test_framework'),
use_module('./${MODULE}'),
main(${base}),
halt.`;

const proc = Bun.spawn(["scryer-prolog", "-g", prologCommand], {
    stdout: "pipe",
    stderr: "pipe"
});

let output = "";

const timeoutId = setTimeout(() => {
    proc.kill();
    process.stdout.write(output);
    console.error(`\n❌ Error: Test timed out after ${TIMEOUT_SECONDS} seconds`);
    process.exit(1);
}, TIMEOUT_SECONDS * 1000);

for await (const chunk of proc.stdout) {
    const text = new TextDecoder().decode(chunk);
    output += text;
    process.stdout.write(chunk);
}

for await (const chunk of proc.stderr) {
    const text = new TextDecoder().decode(chunk);
    output += text;
    process.stderr.write(chunk);
}

const exitCode = await proc.exited;
clearTimeout(timeoutId);

if (output.includes("Failed")) {
    console.error(`\nERROR: test failed`);
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

