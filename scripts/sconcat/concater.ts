import { relative } from "path";

/**
 * Takes a list of files and returns them as concatenated
 * string.
 */
export default async function concatenateFiles(
    files: string[]
): Promise<string> {
    const rootDir = process.cwd();
    let output = '';

    // Add opening <files> tag
    output += '<files>\n';

    for (let i = 0; i < files.length; i++) {
        const file = files[i]!;
        const content = await Bun.file(file).text();
        const relPath = relative(rootDir, file);
        
        output += `<file path="${relPath}">\n${content}\n</file>\n`;
        
        // Add form feed after each file except the last
        if (i < files.length - 1) {
            output += '\f';
        }
    }

    // Add closing </files> tag
    output += '</files>\n';

    return output;
}

