import { watch } from "fs";
import { spawn } from "bun";
import { debounce } from "lodash";

const runTests = async () => {
    const proc = spawn({
        cmd: ["just", "test"],
        stdio: ["inherit", "inherit", "inherit"],
    });
    await proc.exited;
};

const debouncedRunTests = debounce(runTests, 500);

// Initial run
await runTests();

watch("prolog", { recursive: true }, (event, filename) => {
    if (filename?.endsWith(".pl")
        || filename?.endsWith(".plt")) {
        debouncedRunTests();
    }
});

