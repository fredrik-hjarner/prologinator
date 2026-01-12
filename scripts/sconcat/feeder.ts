import { Glob } from "bun";

// ==========================================================
// File collection (filtered by patterns)
// ==========================================================

export default async function collectFilteredFiles(
    include: string[],
    exclude: string[]
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

