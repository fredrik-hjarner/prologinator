import { GoogleGenAI } from '@google/genai';
import { readFile } from 'fs/promises';
import { createInterface } from 'readline';

const ai = new GoogleGenAI({ 
  apiKey: process.env.GEMINI_API_KEY 
});

const formatter = new Intl.NumberFormat('sv-SE', { notation: 'standard' });

async function main() {
  const filePaths: string[] = [];
  
  // 1. Setup Input Listener
  const rl = createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false
  });

  console.log("Paste your file list below. Press Ctrl+D (Mac/Linux) or Ctrl+Z (Win) when done:\n");

  // 2. Read lines until EOF signal
  for await (const line of rl) {
    if (line.trim()) filePaths.push(line.trim());
  }

  console.log(`\nReading content of ${filePaths.length} files...`);

  // 3. Read all files into memory concurrently
  const contents = await Promise.all(
    filePaths.map(async (path) => {
      try {
        // Add a newline to separate files in the token count
        return (await readFile(path, 'utf-8')) + "\n";
      } catch (e) {
        console.warn(`⚠️  Could not read: ${path}`);
        return "";
      }
    })
  );

  // 4. Send one giant request
  // Gemini 2.0 Flash can handle ~1M tokens, so this is safe for most projects.
  const fullText = contents.join("");

  if (fullText.length === 0) {
    console.log("No content found.");
    return;
  }

  const response = await ai.models.countTokens({
    model: "gemini-2.0-flash",
    contents: { 
      role: "user", 
      parts: [{ text: fullText }] 
    },
  });

  console.log(`\nTotal Tokens: ${formatter.format(response.totalTokens)}`);
}

main();