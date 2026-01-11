#!/usr/bin/env bun

import { relative } from "path";
import collectFilteredFiles, { excludePatterns } from "./feeder.ts";
import concatenateFiles from "./concater.ts";
import { InteractiveSelector } from "./Selector.ts";
import { restoreTerminal } from "./terminal.ts";

const DEFAULT_OUTPUT_FILE = "concat.xml";

async function main() {
    try {
        // ==========================================================
        // Stage 1: Exclude Patterns
        // ==========================================================
        const excludeItems = excludePatterns.map(p => ({
            id: p,
            label: p,
            selected: true 
        }));

        const patternSelector = new InteractiveSelector(excludeItems, "Select Exclusion Patterns");
        const activeExcludes = await patternSelector.run();

        if (activeExcludes.length === 0) {
             console.log("No exclusion patterns selected. Proceeding...");
             await new Promise(r => setTimeout(r, 800));
        }

        // ==========================================================
        // Stage 2: Collect Files
        // ==========================================================
        // Pass the selected excludes to the feeder
        const foundFiles = await collectFilteredFiles(undefined, activeExcludes);

        if (foundFiles.length === 0) {
            console.log("No files found matching criteria.");
            process.exit(0);
        }

        // ==========================================================
        // Stage 3: Select & Reorder Files
        // ==========================================================
        const rootDir = process.cwd();
        const fileItems = foundFiles.map(f => ({
            id: f,
            label: relative(rootDir, f),
            selected: true
        }));

        const fileSelector = new InteractiveSelector(fileItems, "Select Files to Concatenate");
        const finalFiles = await fileSelector.run();

        if (finalFiles.length === 0) {
            console.log("No files selected. Exiting.");
            process.exit(0);
        }

        // ==========================================================
        // Stage 4: Generate Output
        // ==========================================================
        restoreTerminal(); // Clean up TTY before standard output
        const outputFile = process.argv[2] || DEFAULT_OUTPUT_FILE;
        console.log(`\nConcatenating ${finalFiles.length} files...`);

        const output = await concatenateFiles(finalFiles);
        await Bun.write(outputFile, output);
        
        console.log(`✓ Created: ${outputFile}`);

    } catch (error) {
        restoreTerminal();
        console.error("An error occurred:", error);
        process.exit(1);
    }
}

main();
