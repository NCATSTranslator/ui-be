import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { run_test_files } from "./lib/file-runner.mjs";

const db_dir = join(dirname(fileURLToPath(import.meta.url)), "db");
const passed = await run_test_files("db", db_dir, process.argv.slice(2));
process.exit(passed ? 0 : 1);
