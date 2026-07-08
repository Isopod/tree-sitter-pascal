const fs = require('fs');
const { execSync } = require('child_process');
const {
    findDcc32, ensureTmpDir, validateToolchain,
    parseExistingCorpus, appendToCorpus, compareWithOracle,
    printReport, cleanup
} = require('./fuzz-shared-code');

const tempFile = 'tmp/fuzz_numbers_temp.pas';
const corpusFile = 'test/corpus/diabolical-numbers.txt';
const TEST_PREFIX = 'Fuzzed Numbers';

function createDelphiProgram(body) {
    return `
program FuzzNumbers;
{$APPTYPE CONSOLE}
var
  I: Int64;
  F: Double;
begin
  ${body}
end.
`;
}

/**
 * The Oracle for Numbers (Separators, Binary, Hex).
 */
function genNumbersWithOracle() {
    const choices = [
        () => { // Decimal with separators
            const parts = [
                Math.floor(Math.random() * 1000).toString(),
                Math.floor(Math.random() * 1000).toString().padStart(3, '0'),
                Math.floor(Math.random() * 1000).toString().padStart(3, '0')
            ];
            const code = parts.join('_');
            return { type: 'I', code, expected: '(literalNumber)' };
        },
        () => { // Hex with separators
            const parts = [
                Math.floor(Math.random() * 255).toString(16).toUpperCase(),
                Math.floor(Math.random() * 255).toString(16).toUpperCase().padStart(2, '0')
            ];
            const code = '$' + parts.join('_');
            return { type: 'I', code, expected: '(literalNumber)' };
        },
        () => { // Binary with separators
            const parts = [
                Math.floor(Math.random() * 15).toString(2),
                Math.floor(Math.random() * 15).toString(2).padStart(4, '0')
            ];
            const code = '%' + parts.join('_');
            return { type: 'I', code, expected: '(literalNumber)' };
        },
        () => { // Float with separators
            const intPart = Math.floor(Math.random() * 1000).toString() + '_' + Math.floor(Math.random() * 1000).toString().padStart(3, '0');
            const fracPart = Math.floor(Math.random() * 1000).toString().padStart(3, '0');
            const code = `${intPart}.${fracPart}`;
            return { type: 'F', code, expected: '(literalNumber)' };
        },
        () => { // Float scientific with separators
            const intPart = Math.floor(Math.random() * 10).toString();
            const fracPart = Math.floor(Math.random() * 1000).toString().padStart(3, '0');
            const exp = Math.floor(Math.random() * 10).toString();
            const code = `${intPart}.${fracPart}e${exp}`;
            return { type: 'F', code, expected: '(literalNumber)' };
        }
    ];

    return choices[Math.floor(Math.random() * choices.length)]();
}

function wrapExpected(innerExpected, varName) {
    return `(root (defProc (declProc (kProcedure) (identifier)) (block (kBegin) (assignment (identifier) (kAssign) ${innerExpected}) (kEnd))))`;
}

// --- Main ---

ensureTmpDir();
const dcc32 = findDcc32();
validateToolchain(dcc32, tempFile, createDelphiProgram("I := 1_000; F := 1_234.567;"));

const { existingCode, maxTestId } = parseExistingCorpus(corpusFile);

const newResults = new Map();
let totalGenerated = 0;
let skippedExisting = 0;

console.log("2. Fuzzing Numbers (Separators/Binary) with Oracle...");
for (let i = 0; i < 200; i++) {
    totalGenerated++;
    const oracle = genNumbersWithOracle();
    const fullCode = `${oracle.type} := ${oracle.code};`;

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

const tempParseFile = `tmp/compare_numbers_${process.pid}.pas`;

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
                expected: fullExpected
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

printReport('Numbers', {
    totalGenerated, skippedExisting,
    compiled: newResults.size,
    passCount, failCount, firstMismatch
});

if (fs.existsSync(tempParseFile)) fs.unlinkSync(tempParseFile);
cleanup(tempFile);
