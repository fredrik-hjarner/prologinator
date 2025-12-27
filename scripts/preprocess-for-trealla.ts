import { file, write } from "bun";

// Get the file path from command line arguments
const filePath = Bun.argv[2];

if (!filePath) {
  console.error("Usage: bun run reorder.ts <path-to-file>");
  process.exit(1);
}

// 1. Regex to find and move to top
const moveRegex = /^(builtin_action[(].*[)][.])/;

// 2. String/Regex to delete completely
const deletePattern = ":- discontiguous(builtin_action/1).";

async function processFile(path: string) {
  try {
    const f = file(path);
    const text = await f.text();

    // Split content into lines to process them individually
    // using split(/\r?\n/) handles both Windows and Unix
    // line endings during read
    const lines = text.split(/\r?\n/);

    const matches: string[] = [];
    const others: string[] = [];

    for (const line of lines) {
      // Check if line should be deleted first
      if (line.includes(deletePattern)) {
        continue; // Skip this line entirely
      }

      // Check if line matches the "move to top" regex
      if (moveRegex.test(line)) {
        matches.push(line);
      } else {
        others.push(line);
      }
    }

    // Combine: Matches at top + Rest of file
    const newContent = [...matches, ...others].join("\n");

    // Overwrite the file
    await write(path, newContent);
    console.log(`Successfully processed: ${path}`);

  } catch (error) {
    console.error(`Error processing file: ${error}`);
  }
}

await processFile(filePath);