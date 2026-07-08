const fs = require('fs');
const { execSync } = require('child_process');
const path = require('path');

/**
 * Auto-detect dcc32 path by scanning Embarcadero Studio installations.
 * Prefers the highest BDS version found.
 */
function findDcc32() {
    const studioRoot = 'C:\\Program Files (x86)\\Embarcadero\\Studio';
    try {
        const versions = fs.readdirSync(studioRoot)
            .filter(d => /^\d+\.\d+$/.test(d))
            .sort((a, b) => parseFloat(b) - parseFloat(a));
        for (const ver of versions) {
            const p = path.join(studioRoot, ver, 'bin', 'dcc32.EXE');
            if (fs.existsSync(p)) return `"${p}"`;
        }
    } catch (e) { /* fall through */ }
    return `"${path.join(studioRoot, '37.0', 'bin', 'dcc32.EXE')}"`;
}

/**
 * Ensure the tmp/ directory exists at project root.
 */
function ensureTmpDir() {
    fs.mkdirSync('tmp', { recursive: true });
}

/**
 * Compile known-good code to verify the Delphi toolchain works.
 * Exits the process on failure.
 */
function validateToolchain(dcc32, tempFile, knownGoodCode) {
    console.log("1. Validating toolchain...");
    fs.writeFileSync(tempFile, knownGoodCode);
    try {
        execSync(`${dcc32} -B -CC -W- -H- ${tempFile}`, { stdio: 'ignore' });
        console.log("   [PASS] Toolchain is valid.");
    } catch (e) {
        console.error("   [FAIL] Toolchain validation failed!");
        process.exit(1);
    }
}

/**
 * Parse an existing tree-sitter corpus file.
 * Returns the set of code blocks (for dedup), the highest test ID, and raw content.
 *
 * Corpus format:
 *   ===
 *   Test Name
 *   ===
 *   code...
 *   ---
 *   expected AST...
 *   (blank line)
 */
function parseExistingCorpus(corpusFile) {
    if (!fs.existsSync(corpusFile)) {
        return { existingCode: new Set(), maxTestId: 0 };
    }

    const content = fs.readFileSync(corpusFile, 'utf8');
    const existingCode = new Set();
    let maxTestId = 0;

    // Split on === delimiter lines. The pattern alternates: name, body, name, body...
    const parts = content.split(/^===$/m);

    for (let i = 1; i < parts.length - 1; i += 2) {
        const name = parts[i].trim();
        const body = parts[i + 1];
        if (!body) continue;

        // Extract numeric ID from test name (e.g., "Fuzzed Ternary 408" → 408)
        const idMatch = name.match(/(\d+)\s*$/);
        if (idMatch) {
            maxTestId = Math.max(maxTestId, parseInt(idMatch[1]));
        }

        // Extract code block (everything before the --- separator)
        const dashIndex = body.indexOf('\n---');
        if (dashIndex >= 0) {
            const code = body.substring(0, dashIndex).replace(/\r\n/g, '\n').trim();
            existingCode.add(code);
        }
    }

    return { existingCode, maxTestId };
}

/**
 * Append new test entries to a corpus file without destroying existing content.
 * Each entry: { name, code, expected }
 */
function appendToCorpus(corpusFile, newTests) {
    if (newTests.length === 0) return;

    let content = '';
    for (const test of newTests) {
        content += `===\n${test.name}\n===\n${test.code}\n---\n${test.expected}\n\n`;
    }

    if (fs.existsSync(corpusFile) && fs.readFileSync(corpusFile, 'utf8').trim().length > 0) {
        // Append to existing file
        fs.appendFileSync(corpusFile, content);
    } else {
        fs.writeFileSync(corpusFile, content);
    }
}

/**
 * Normalize tree-sitter parse output into a single-line S-expression
 * without position ranges or field labels.
 *
 * Field labels (like "name:", "body:") always precede "(" in tree-sitter output,
 * so stripping "word: " before "(" is safe — AST node names never contain ": ".
 */
function cleanParserOutput(raw) {
    return raw
        .replace(/\s*\[\d+,\s*\d+\]\s*-\s*\[\d+,\s*\d+\]/g, '')  // strip position ranges
        .replace(/\w+:\s+/g, '')                                     // strip field labels
        .replace(/\s+/g, ' ')                                        // collapse whitespace
        .replace(/\(root\(/g, '(root (')                             // normalize root spacing
        .trim();
}

/**
 * Compare new fuzzer results against the oracle by parsing each test case
 * with tree-sitter and checking the AST.
 *
 * @param {Map} newResults - Map of innerCode → oracleExpectedAST
 * @param {function} wrapExpected - (innerExpected) => full expected S-expression
 * @returns {{ passCount, failCount, firstMismatch }}
 */
function compareWithOracle(newResults, wrapExpected) {
    let passCount = 0;
    let failCount = 0;
    let firstMismatch = null;

    // Use a unique temp file to avoid conflicts when running fuzzers in parallel
    const tempParseFile = `tmp/compare_${process.pid}_${Date.now()}.pas`;

    newResults.forEach((expectedAST, code) => {
        const testCode = `procedure Test; begin ${code} end;`;
        fs.writeFileSync(tempParseFile, testCode);

        try {
            const output = execSync(`npx tree-sitter parse ${tempParseFile}`, { encoding: 'utf8' }).trim();
            const actualAST = cleanParserOutput(output);

            const fullExpected = wrapExpected(expectedAST)
                .replace(/\s+/g, ' ')
                .trim();

            if (actualAST === fullExpected && !actualAST.includes('ERROR') && !actualAST.includes('MISSING')) {
                passCount++;
            } else {
                if (!firstMismatch) {
                    firstMismatch = { code: testCode, expected: fullExpected, actual: actualAST };
                }
                failCount++;
            }
        } catch (e) {
            failCount++;
        }
    });

    // Cleanup
    if (fs.existsSync(tempParseFile)) fs.unlinkSync(tempParseFile);

    return { passCount, failCount, firstMismatch };
}

/**
 * Print the standard diabolical report.
 */
function printReport(label, { totalGenerated, skippedExisting, compiled, passCount, failCount, firstMismatch }) {
    console.log(`\nDIABOLICAL REPORT: ${label}`);
    console.log("=================");
    console.log(`Generated:         ${totalGenerated}`);
    if (skippedExisting > 0) {
        console.log(`Already in corpus: ${skippedExisting}`);
    }
    console.log(`Compiled (Valid):  ${compiled}`);
    console.log(`Matched Oracle:    ${passCount}`);
    console.log(`Mismatched Oracle: ${failCount}`);

    const yieldRate = (compiled / totalGenerated) * 100;
    console.log(`\nYield Rate:        ${yieldRate.toFixed(1)}% (Target: >20%)`);

    if (compiled > 0) {
        const failRate = (failCount / compiled) * 100;
        console.log(`Grammar Fail Rate: ${failRate.toFixed(1)}%`);
    }

    if (firstMismatch) {
        console.log("\nFIRST MISMATCH:");
        console.log("  CODE:     ", firstMismatch.code);
        console.log("  EXPECTED: ", firstMismatch.expected);
        console.log("  ACTUAL:   ", firstMismatch.actual);
    }

    if (failCount > 0) {
        console.log(`\n[RESULT] Exposed ${failCount} cases where the parser and oracle disagree.`);
    } else if (compiled > 0) {
        console.log("\n[RESULT] Parser matches Oracle perfectly. Consider increasing complexity.");
    }
}

/**
 * Clean up temporary build artifacts.
 */
function cleanup(tempFile) {
    if (fs.existsSync(tempFile)) fs.unlinkSync(tempFile);
    // Clean up compiler output (.exe, .dcu, .obj) based on the temp file base name
    const base = path.basename(tempFile, '.pas');
    for (const ext of ['.exe', '.dcu', '.obj']) {
        const f = path.join(path.dirname(tempFile), base + ext);
        if (fs.existsSync(f)) fs.unlinkSync(f);
    }
}

module.exports = {
    findDcc32,
    ensureTmpDir,
    validateToolchain,
    parseExistingCorpus,
    appendToCorpus,
    cleanParserOutput,
    compareWithOracle,
    printReport,
    cleanup
};
