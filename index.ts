import SWIPL from "swipl-wasm";
import { readFileSync } from "fs";
import { join } from "path";

async function main() {
  console.log("Initializing SWI-Prolog WASM...");
  
  const swipl = await SWIPL({ 
    arguments: ["-q"],
    print: (text: string) => {
      // In WASM mode, Prolog sends:
      // - "char\n" for each character (we strip the \n and print just the char)
      // - "@\n" for real newlines (we output \n)
      if (text === "@") {
        // Real newline marker - output actual newline
        process.stdout.write("\n");
        return;
      }
      process.stdout.write(text);
    },
    printErr: (text: string) => process.stderr.write(text),
  });

  console.log("Mounting Prolog files to virtual filesystem...");

  // Create the prolog directory in the virtual filesystem
  swipl.FS.mkdir("/prolog");

  // Mount all Prolog files to the virtual FS
  const prologDir = "./prolog";
  const files = [
    "config.pl",
    "typewriter.pl",
    "operators.pl",
    "outcomes.pl",
    "combat_narration.pl",
    "main_wasm.pl",
    "game.pl",
  ];

  for (const file of files) {
    const filePath = join(prologDir, file);
    const content = readFileSync(filePath, "utf-8");
    console.log(`Mounting ${file}...`);
    
    // Write file to virtual filesystem
    swipl.FS.writeFile(`/prolog/${file}`, content);
  }

  console.log("\nLoading Prolog modules...");
  
  try {
    // Set environment variables before loading modules
    swipl.prolog.call("setenv('WASM', 'true')");  // Enable WASM mode for typewriter
    // TODO: I should have a separate AUTOSTART env var to not mix it with
    //       debug mode.
    // swipl.prolog.call("setenv('DEBUG', '1')"); // Prevent auto-start
    
    // Now load the WASM-specific entry point
    swipl.prolog.call("consult('/prolog/main_wasm.pl')");
    
    console.log("Modules loaded successfully!\n");
  } catch (error) {
    console.error("Error loading modules:", error);
    throw error;
  }

  console.log("ℹ️  Note: Running in AI vs AI mode (interactive input doesn't work in WASM)\n");
  console.log("Starting auto-battle...\n");

  try {
    const result = swipl.prolog.query("play_auto_battle").once();
    console.log("\n✓ Battle completed!");
  } catch (error) {
    console.error("\n✗ Error running battle:", error);
    console.log("\n💡 For interactive gameplay with keyboard input, use: bun run start");
  }
}

main().catch(console.error);
