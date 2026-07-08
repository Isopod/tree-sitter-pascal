const fs = require('fs');
const { execSync } = require('child_process');
const {
    findDcc32, ensureTmpDir, validateToolchain,
    parseExistingCorpus, appendToCorpus, compareWithOracle,
    printReport, cleanup
} = require('./fuzz-shared-code');

const tempFile = 'tmp/fuzz_ternary_temp.pas';
const corpusFile = 'test/corpus/diabolical-ternary.txt';
const TEST_PREFIX = 'Fuzzed Ternary';

function createDelphiProgram(body) {
    return `
program FuzzTernary;
{$APPTYPE CONSOLE}
var
  A, B, C, D, Result: Integer;
begin
  A := 1; B := 2; C := 3; D := 4;
  ${body}
end.
`;
}

/**
 * The Oracle: Generates both Code and its Expected Tree-sitter S-expression.
 *
 * Conditions like (A < B) produce exprParens wrapping exprBinary in the grammar,
 * so the oracle must include the exprParens wrapper.
 */
function genTernaryWithOracle(depth) {
    if (depth <= 0) {
        const id = ['A', 'B', 'C', 'D'][Math.floor(Math.random() * 4)];
        return { code: id, expected: `(identifier)` };
    }

    const choice = Math.floor(Math.random() * 3);

    if (choice === 0) { // exprIf
        const cond = [`(A < B)`, `(C > D)`, `True`, `False`][Math.floor(Math.random() * 4)];
        const condExpected = cond.includes('<') ? `(exprParens (exprBinary (identifier) (kLt) (identifier)))` :
                             cond.includes('>') ? `(exprParens (exprBinary (identifier) (kGt) (identifier)))` :
                             cond === 'True' ? `(kTrue)` : `(kFalse)`;

        const v1 = genTernaryWithOracle(depth - 1);
        const v2 = genTernaryWithOracle(depth - 1);

        return {
            code: `if ${cond} then ${v1.code} else ${v2.code}`,
            expected: `(exprIf (kIf) ${condExpected} (kThen) ${v1.expected} (kElse) ${v2.expected})`
        };
    } else if (choice === 1) { // exprParens
        const inner = genTernaryWithOracle(depth - 1);
        return {
            code: `(${inner.code})`,
            expected: `(exprParens ${inner.expected})`
        };
    } else { // exprBinary (+)
        let left = genTernaryWithOracle(depth - 1);
        let right = genTernaryWithOracle(depth - 1);

        // Wrap left-side exprIf in parens: without parens, `if X then A else B + C`
        // parses as `if X then A else (B + C)` because + (prec 20) > if (prec -1).
        if (left.expected.startsWith('(exprIf ')) {
            left = { code: `(${left.code})`, expected: `(exprParens ${left.expected})` };
        }

        // Wrap right-side exprBinary/exprIf in parens: without parens, `A + B + C`
        // parses left-to-right as `(A + B) + C`, not the oracle's `A + (B + C)`.
        if (right.expected.startsWith('(exprBinary ') || right.expected.startsWith('(exprIf ')) {
            right = { code: `(${right.code})`, expected: `(exprParens ${right.expected})` };
        }

        return {
            code: `${left.code} + ${right.code}`,
            expected: `(exprBinary ${left.expected} (kAdd) ${right.expected})`
        };
    }
}

function wrapExpected(innerExpected) {
    return `(root (defProc (declProc (kProcedure) (identifier)) (block (kBegin) (assignment (identifier) (kAssign) ${innerExpected}) (kEnd))))`;
}

// --- Main ---

ensureTmpDir();
const dcc32 = findDcc32();
validateToolchain(dcc32, tempFile, createDelphiProgram('Result := if A < B then C else D;'));

const { existingCode, maxTestId } = parseExistingCorpus(corpusFile);

const newResults = new Map();
let totalGenerated = 0;
let skippedExisting = 0;

console.log("2. Fuzzing Ternary Expressions with Oracle...");
for (let i = 0; i < 500; i++) {
    totalGenerated++;
    const oracle = genTernaryWithOracle(Math.floor(Math.random() * 4) + 1);
    const fullCode = `Result := ${oracle.code};`;

    if (newResults.has(fullCode)) continue;

    // Check against existing corpus
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

const tempParseFile = `tmp/compare_ternary_${process.pid}.pas`;

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

printReport('Ternary Expressions', {
    totalGenerated, skippedExisting,
    compiled: newResults.size,
    passCount, failCount, firstMismatch
});

if (fs.existsSync(tempParseFile)) fs.unlinkSync(tempParseFile);
cleanup(tempFile);
