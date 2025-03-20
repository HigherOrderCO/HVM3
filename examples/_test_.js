/**
 * HVM Test Script
 *
 * This script tests the HVM implementation by running a series of .hvm files
 * in both interpreted and compiled modes, as specified in the test table.
 * For each test case:
 * - If a 'main' line is provided, it replaces the '@main' line in the original file
 *   and saves it as '_test_.hvm'. Otherwise, it copies the original file to '_test_.hvm'.
 * - Runs the test 4 times:
 *   - First run: Extracts the result (first line of output) and checks it against
 *     the expected 'norm'. Logs an error if they don't match.
 *   - Next 3 runs: Measures the execution time (from the 'TIME:' line in output).
 * - Averages the times from the last 3 runs.
 * - Compares the average time with the previous run's time stored in '_perf_.json'.
 *   If the new time is >5% slower and the total run time is >= 0.05 seconds, logs a warning.
 * - Updates '_perf_.json' with the new average times after all tests.
 *
 * Key Notes:
 * - Only runs modes ('intr' or 'comp') specified in the test table for each file.
 * - Some tests lack a 'main' line, meaning no replacement is needed—just copy the file.
 * - Uses Node.js 'fs' for file operations and 'child_process.execSync' to run commands.
 * - Assumes the script runs in a directory containing the .hvm files.
 * - Commands use a hardcoded project directory (/Users/v/vic/dev/HVM) for interpreted mode.
 */

const fs = require('fs');
const { execSync } = require('child_process');

// ### Test Specifications
// Array of test objects, each defining a file and its test cases for 'intr' (interpreted)
// and/or 'comp' (compiled) modes. Each mode object has an expected 'norm' and an optional 'main'.
const tests = [
  {
    file: 'bench_cnots.hvm',
    intr: { main: '@main = (@P20 @not @true)', norm: 'λa λb a' },
    comp: { main: '@main = (@P24 @not @true)', norm: 'λa λb a' }
  },
  {
    file: 'bench_count.hvm',
    intr: { main: '@main = @count(2_000_000 0)', norm: '4000000' },
    comp: { main: '@main = @count(2_000_000_000 0)', norm: '4000000000' }
  },
  {
    file: 'bench_sum_range.hvm',
    intr: { main: '@main = @sum(@range(1_000_000 #Nil) 0)', norm: '1783293664' },
    comp: { main: '@main = @sum(@range(300_000_000 #Nil) 0)', norm: '3992170112' }
  },
  {
    file: 'enum_coc_smart.hvm',
    intr: { norm: '"λλ(0 λλ((1 3) 2))"' },
    comp: { norm: '"λλ(0 λλ((1 3) 2))"' }
  },
  {
    file: 'enum_lam_naive_blc.hvm',
    comp: { norm: '"λλ(1 λλ((2 0) 1))"' }
  },
  {
    file: 'enum_lam_smart.hvm',
    intr: { norm: '"λ(0 λλλ((0 1) 2))"' },
    comp: { norm: '"λ(0 λλλ((0 1) 2))"' }
  },
  {
    file: 'enum_nat.hvm',
    intr: { main: '@main = @if(@eq(@mul(@X @nat(20)) @nat(12000)) @u32(@X) *)', norm: '600' },
    comp: { main: '@main = @if(@eq(@mul(@X @nat(20)) @nat(20000)) @u32(@X) *)', norm: '1000' }
  },
  {
    file: 'enum_primes.hvm',
    intr: { main: '@main = @if(@eq(@mul(@X_B @Y_B) @P_B) λt(t @u32(@X_B) @u32(@Y_B)) *)', norm: 'λa ((a 853) 947)' },
    comp: { main: '@main = @if(@eq(@mul(@X_C @Y_C) @P_C) λt(t @u32(@X_C) @u32(@Y_C)) *)', norm: 'λa ((a 25997) 27299)' }
  },
  {
    file: 'feat_affine_ctx.hvm',
    intr: { norm: '1' },
    comp: { norm: '1' }
  },
  {
    file: 'feat_cmul.hvm',
    intr: { norm: 'λa λb (a (a (a (a b))))' },
    comp: { norm: 'λa λb (a (a (a (a b))))' }
  },
  {
    file: 'feat_hoas.hvm',
    intr: { norm: '"λx λy (x (x (x (x y))))"' },
    comp: { norm: '"λx λy (x (x (x (x y))))"' }
  },
  {
    file: 'feat_mut_ref.hvm',
    intr: { norm: '2' },
    comp: { norm: '2' }
  },
  {
    file: 'fuse_inc.hvm',
    intr: { norm: '1234567' },
    comp: { norm: '1234567' }
  },
  {
    file: 'fuse_mul.hvm',
    intr: { main: '@main = @mul(12345 12345)', norm: '152399025' },
    comp: { main: '@main = @mul(23232 32323)', norm: '750927936' }
  },
  {
    file: 'fuse_rot.hvm',
    intr: { main: '@main = (@read(@S) @sqr(12345 (@add(@S) @KA) @KB))', norm: '209865' },
    comp: { main: '@main = (@read(@S) @sqr(54321 (@add(@S) @KA) @KB))', norm: '923457' }
  }
];

// ### Load Previous Performance Data
// Load '_perf_.json' if it exists, otherwise initialize an empty object.
let perfData = {};
try {
  perfData = JSON.parse(fs.readFileSync('_perf_.json', 'utf8'));
} catch (e) {
  console.log('[INFO] _perf_.json not found, initializing as empty.');
}

// ### Counters for Errors and Warnings
// Track the number of errors (result mismatches) and warnings (perf regressions).
let errorCount = 0;
let warningCount = 0;

// ### Main Test Loop
for (const test of tests) {
  const file = test.file;
  // Check each mode: 'intr' (interpreted) and 'comp' (compiled).
  for (const mode of ['intr', 'comp']) {
    if (test[mode]) { // Only run if the mode is specified in the test.
      console.log(`Running ${file} in ${mode} mode...`);
      const { main, norm } = test[mode];

      // Prepare the test file by adjusting '@main' or copying as needed.
      prepareTestFile(file, main);

      let times = [];
      for (let i = 0; i < 4; i++) {
        const output = runTest(mode);
        const { result, time } = parseOutput(output);

        console.log("- time:", time.toFixed(7), "| norm: " + result);

        if (i === 0) {
          // First run: Check the result against the expected norm.
          if (result !== norm) {
            console.log(`[ERROR] For ${file} in ${mode} mode, expected "${norm}", but got "${result}"`);
            errorCount++;
          }
        } else {
          // Next 3 runs: Collect execution times.
          times.push(time);
        }
      }

      // Calculate average time from the last 3 runs.
      const averageTime = times.reduce((a, b) => a + b, 0) / times.length;
      const key = `${file}_${mode}`; // Unique key for this test and mode.
      const previousTime = perfData[key];

      // Check for performance regression (>5% slower) if previous data exists and time >= 0.05s.
      if (previousTime && averageTime > previousTime * 1.05 && averageTime >= 0.05) {
        console.log(`[WARNING] Performance regression for ${file} in ${mode} mode: ` +
                    `previous ${previousTime.toFixed(6)}s, now ${averageTime.toFixed(6)}s`);
        warningCount++;
      }

      // Update performance data with the new average time.
      perfData[key] = averageTime;
    }
  }
}

// ### Save Updated Performance Data
// Write the new performance data to '_perf_.json' with readable formatting.
fs.writeFileSync('_perf_.json', JSON.stringify(perfData, null, 2), 'utf8');

// ### Summary
// Log the total number of errors and warnings.
console.log(`All tests completed with ${errorCount} errors and ${warningCount} warnings.`);

// ### Helper Functions

/**
 * Prepares the test file by replacing the '@main' line or copying the original file.
 * @param {string} originalFile - The original .hvm file path.
 * @param {string|undefined} mainLine - The new '@main' line to use, if provided.
 */
function prepareTestFile(originalFile, mainLine) {
  if (mainLine) {
    // Read the original file and split into lines.
    const lines = fs.readFileSync(originalFile, 'utf8').split('\n');
    // Find the line starting with '@main'.
    const index = lines.findIndex(line => line.startsWith('@main'));
    if (index !== -1) {
      lines[index] = mainLine; // Replace the '@main' line.
    } else {
      // If no '@main' line exists, append the new one and warn.
      console.log(`[WARNING] No @main line found in ${originalFile}, adding at the end.`);
      lines.push(mainLine);
    }
    // Save the modified content to '_test_.hvm'.
    fs.writeFileSync('_test_.hvm', lines.join('\n'), 'utf8');
  } else {
    // If no mainLine is provided, copy the original file as is.
    fs.copyFileSync(originalFile, '_test_.hvm');
  }
}

/**
 * Runs the HVM test in the specified mode and returns the output.
 * @param {string} mode - 'intr' for interpreted, 'comp' for compiled.
 * @returns {string} - The command output.
 */
function runTest(mode) {
  let command;
  if (mode === 'intr') {
    command = 'cabal run -v0 hvm --project-dir=/Users/v/vic/dev/HVM -- run _test_.hvm -C -s';
  } else if (mode === 'comp') {
    command = 'hvm run _test_.hvm -c -C -s';
  } else {
    throw new Error(`Unknown mode: ${mode}`);
  }
  // Execute the command and capture the output as a UTF-8 string.
  return execSync(command, { encoding: 'utf8' });
}

/**
 * Parses the command output to extract the result and time.
 * @param {string} output - The output from running the HVM command.
 * @returns {{result: string, time: number}} - The result (first line) and time in seconds.
 */
function parseOutput(output) {
  const lines = output.split('\n');
  const result = lines[0].trim(); // First line is the result.
  const timeLine = lines.find(line => line.startsWith('TIME:'));
  if (!timeLine) {
    throw new Error('TIME line not found in output');
  }
  const timeStr = timeLine.split(' ')[1]; // Extract time value after 'TIME:'.
  const time = parseFloat(timeStr);
  if (isNaN(time)) {
    throw new Error(`Failed to parse time: ${timeStr}`);
  }
  return { result, time };
}
