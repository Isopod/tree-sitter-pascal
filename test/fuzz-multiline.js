const fs = require('fs');
const { execSync } = require('child_process');
const {
    findDcc32, ensureTmpDir, validateToolchain,
    parseExistingCorpus, appendToCorpus, compareWithOracle,
    printReport, cleanup
} = require('./fuzz-shared-code');

const tempFile = 'tmp/fuzz_multiline_temp.pas';
const corpusFile = 'test/corpus/diabolical-multiline.txt';
const TEST_PREFIX = 'Fuzzed Multiline';

function createDelphiProgram(body) {
    return `
program FuzzMultiline;
{$APPTYPE CONSOLE}
var
  S: string;
  A, B: Integer;
begin
  ${body}
end.
`;
}

/**
 * The Oracle for multi-line string literals (triple-quoted).
 */
function genMultilineWithOracle() {
    const contents = [
        "",
        "Hello World",
        "Line 1\nLine 2",
        "Contains 'single quotes'",
        "Contains ''double quotes''",
        "Contains keywords: if then else begin end",
        "   Indented content",
        "\n\n\nMultiple newlines\n\n\n",
    ];

    const content = contents[Math.floor(Math.random() * contents.length)];
    const code = `'''${content}'''`;
    const expected = content === ""
        ? `(literalStringMultiline)`
        : `(literalStringMultiline (stringContent))`;

    return { code, expected };
}

function genComplexExpr(depth) {
    if (depth <= 0) {
        if (Math.random() < 0.5) {
            return genMultilineWithOracle();
        } else {
            return { code: "'normal string'", expected: "(literalString)" };
        }
    }

    const choice = Math.floor(Math.random() * 3);
    if (choice === 0) { // Concatenation
        let left = genComplexExpr(depth - 1);
        let right = genComplexExpr(depth - 1);

        // Same precedence/associativity wrapping as fuzz-ternary.js
        if (left.expected.startsWith('(exprIf ')) {
            left = { code: `(${left.code})`, expected: `(exprParens ${left.expected})` };
        }
        if (right.expected.startsWith('(exprBinary ') || right.expected.startsWith('(exprIf ')) {
            right = { code: `(${right.code})`, expected: `(exprParens ${right.expected})` };
        }

        return {
            code: `${left.code} + ${right.code}`,
            expected: `(exprBinary ${left.expected} (kAdd) ${right.expected})`
        };
    } else if (choice === 1) { // Ternary with multiline
        const left = genComplexExpr(depth - 1);
        const right = genComplexExpr(depth - 1);
        return {
            code: `if True then ${left.code} else ${right.code}`,
            expected: `(exprIf (kIf) (kTrue) (kThen) ${left.expected} (kElse) ${right.expected})`
        };
    } else {
        return genMultilineWithOracle();
    }
}

function wrapExpected(innerExpected) {
    return `(root (defProc (declProc (kProcedure) (identifier)) (block (kBegin) (assignment (identifier) (kAssign) ${innerExpected}) (kEnd))))`;
}

// --- Main ---

ensureTmpDir();
const dcc32 = findDcc32();
validateToolchain(dcc32, tempFile, createDelphiProgram("S := '''Initial test''';"));

const { existingCode, maxTestId } = parseExistingCorpus(corpusFile);

const newResults = new Map();
let totalGenerated = 0;
let skippedExisting = 0;

console.log("2. Fuzzing Multi-line Strings with Oracle...");
for (let i = 0; i < 300; i++) {
    totalGenerated++;
    const oracle = genComplexExpr(Math.floor(Math.random() * 3));
    const fullCode = `S := ${oracle.code};`;

    if (newResults.has(fullCode)) continue;

    const codeBlock = `procedure Test;\nbegin\n  ${fullCode}\nend;`;
    if (existingCode.has(codeBlock)) { skippedExisting++; continue; }

    fs.writeFileSync(tempFile, createDelphiProgram(fullCode));
    try {
        execSync(`${dcc32} -B -CC -W- -H- ${tempFile}`, { stdio: 'ignore' });
        newResults.set(fullCode, oracle.expected);
        process.stdout.write('+');
    } catch (e) {
        process.stdout.write('.');
    }
}

console.log(`\n\nGenerated: ${totalGenerated}`);
console.log(`Already in corpus: ${skippedExisting}`);
console.log(`New unique compiled: ${newResults.size}`);

// 3. Running Tree-sitter Comparison...
console.log("\n3. Running Tree-sitter Comparison and generating corpus...");
let passCount = 0;
let failCount = 0;
let firstMismatch = null;
const newTests = [];
let testId = maxTestId;

const tempParseFile = `tmp/compare_multiline_${process.pid}.pas`;

newResults.forEach((expectedInner, code) => {
    testId++;
    const fullCode = `procedure Test;\nbegin\n  ${code}\nend;`;
    fs.writeFileSync(tempParseFile, fullCode + '\n');

    const fullExpected = wrapExpected(expectedInner).replace(/\s+/g, ' ').trim();

    try {
        const output = execSync(`npx tree-sitter parse ${tempParseFile}`, { encoding: 'utf8' }).trim();
        const actual = cleanParserOutput(output);

        if (actual.includes('ERROR') || actual.includes('MISSING') || actual !== fullExpected) {
            failCount++;
            if (!firstMismatch) firstMismatch = { code, expected: fullExpected, actual };
            newTests.push({
                name: `${TEST_PREFIX} ${testId}`,
                code: fullCode,
                expected: fullExpected // Use Oracle's expected AST for the failure
            });
            process.stdout.write('F');
        } else {
            passCount++;
            newTests.push({
                name: `${TEST_PREFIX} ${testId}`,
                code: fullCode,
                expected: fullExpected
            });
            process.stdout.write('.');
        }
    } catch (e) {
        failCount++;
        newTests.push({
            name: `${TEST_PREFIX} ${testId}`,
            code: fullCode,
            expected: fullExpected
        });
        process.stdout.write('X');
    }
});

appendToCorpus(corpusFile, newTests);
console.log(`\nAppended ${newTests.length} tests to ${corpusFile}`);

printReport('Multi-line Strings', {
    totalGenerated, skippedExisting,
    compiled: newResults.size,
    passCount, failCount, firstMismatch
});

if (fs.existsSync(tempParseFile)) fs.unlinkSync(tempParseFile);
cleanup(tempFile);
