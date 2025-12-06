import { Glob } from "bun";

// ==========================================================
// Configuration: Edit these patterns at the top of the file
// ==========================================================

// Array of glob patterns to include (e.g., ["**/*.pl", "**/*.ts"])
export const includePatterns: string[] = [
    "**/*.pl",
    "**/*.ts"
];

// Array of glob patterns to exclude (e.g., ["**/node_modules/**", "**/*.test.ts"])
export const excludePatterns: string[] = [
    "**/.git/**",
    "**/.cursor/**",
    "**/.husky/**",
    "**/.vscode/**",
    // "**/internal_docs/**",
    "**/node_modules/**",
    // "**/scripts/**",
    "**/submodules/**",
];

// ==========================================================
// File collection (filtered by patterns)
// ==========================================================

export default async function collectFilteredFiles(
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

