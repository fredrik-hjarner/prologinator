import { GoogleGenAI } from '@google/genai';
import { readFile } from 'fs/promises';

const ai = new GoogleGenAI({ 
  apiKey: process.env.GEMINI_API_KEY 
});

const formatter = new Intl.NumberFormat(
    'sv-SE',
    // { notation: 'compact' }
    { notation: 'standard' }
);

async function countFileTokens(filePath: string): number {
  const content = await readFile(filePath, 'utf-8');
  
  const response = await ai.models.countTokens({
    model: "gemini-2.0-flash",
    contents: content,
  });
  
  return response.totalTokens;
}

const filePath = process.argv[2];
if (!filePath) {
  console.error('Usage: bun script.ts <file-path>');
  process.exit(1);
}

const tokens = await countFileTokens(filePath);
console.log(`${formatter.format(tokens)}`);