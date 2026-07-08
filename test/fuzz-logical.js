const fs = require('fs');
const { execSync } = require('child_process');
const {
    findDcc32, ensureTmpDir, validateToolchain,
    parseExistingCorpus, appendToCorpus, compareWithOracle,
    printReport, cleanup
} = require('./fuzz-shared-code');

const tempFile = 'tmp/fuzz_logical_temp.pas';
const corpusFile = 'test/corpus/diabolical-logical.txt';
const TEST_PREFIX = 'Fuzzed Logical';

function createDelphiProgram(body) {
    return `
program FuzzLogical;
{$APPTYPE CONSOLE}
type
  TMyClass = class end;
var
  A, B, C, D: Integer;
  Obj: TObject;
  BVal: Boolean;
begin
  A := 1; B := 2; C := 3; D := 4;
  Obj := TMyClass.Create;
  ${body}
end.
`;
}

/**
 * The Oracle for logical operators (is not, not in, and, or, not).
 * All expressions are parenthesized in code, and the oracle correctly
 * wraps them in exprParens.
 */
function genLogicalWithOracle(depth) {
    if (depth <= 0) {
        return { code: 'BVal', expected: '(identifier)' };
    }

    const choice = Math.floor(Math.random() * 5);
    if (choice === 0) { // is not
        return {
            code: `(Obj is not TMyClass)`,
            expected: `(exprParens (exprBinary (identifier) (kIsNot) (identifier)))`
        };
    } else if (choice === 1) { // not in
        return {
            code: `(A not in [1, 2, 3])`,
            expected: `(exprParens (exprBinary (identifier) (kNotIn) (exprBrackets (literalNumber) (literalNumber) (literalNumber))))`
        };
    } else if (choice === 2) { // and / or
        const left = genLogicalWithOracle(depth - 1);
        const right = genLogicalWithOracle(depth - 1);
        const op = Math.random() < 0.5 ? 'and' : 'or';
        const kOp = op === 'and' ? '(kAnd)' : '(kOr)';
        return {
            code: `(${left.code} ${op} ${right.code})`,
            expected: `(exprParens (exprBinary ${left.expected} ${kOp} ${right.expected}))`
        };
    } else if (choice === 3) { // not
        const inner = genLogicalWithOracle(depth - 1);
        return {
            code: `(not ${inner.code})`,
            expected: `(exprParens (exprUnary (kNot) ${inner.expected}))`
        };
    } else {
        return { code: 'True', expected: '(kTrue)' };
    }
}

function wrapExpected(innerExpected) {
    return `(root (defProc (declProc (kProcedure) (identifier)) (block (kBegin) (assignment (identifier) (kAssign) ${innerExpected}) (kEnd))))`;
}

// --- Main ---

ensureTmpDir();
const dcc32 = findDcc32();
validateToolchain(dcc32, tempFile, createDelphiProgram("BVal := (Obj is not TMyClass) and (A not in [1]);"));

const { existingCode, maxTestId } = parseExistingCorpus(corpusFile);

const newResults = new Map();
let totalGenerated = 0;
let skippedExisting = 0;

console.log("2. Fuzzing Logical Operators with Oracle...");
for (let i = 0; i < 300; i++) {
    totalGenerated++;
    const oracle = genLogicalWithOracle(Math.floor(Math.random() * 3) + 1);
    const fullCode = `BVal := ${oracle.code};`;

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

const tempParseFile = `tmp/compare_logical_${process.pid}.pas`;

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

printReport('Logical Operators', {
    totalGenerated, skippedExisting,
    compiled: newResults.size,
    passCount, failCount, firstMismatch
});

if (fs.existsSync(tempParseFile)) fs.unlinkSync(tempParseFile);
cleanup(tempFile);
