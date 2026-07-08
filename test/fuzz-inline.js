const fs = require('fs');
const { execSync } = require('child_process');
const {
    findDcc32, ensureTmpDir, validateToolchain,
    parseExistingCorpus, appendToCorpus, compareWithOracle,
    printReport, cleanup
} = require('./fuzz-shared-code');

const tempFile = 'tmp/fuzz_inline_temp.pas';
const corpusFile = 'test/corpus/diabolical-inline.txt';
const TEST_PREFIX = 'Fuzzed Inline';

function createDelphiProgram(body) {
    return `
program FuzzInline;
{$APPTYPE CONSOLE}
var
  G: Integer;
begin
  ${body}
end.
`;
}

/**
 * The Oracle for inline variable/constant declarations.
 *
 * Grammar produces:
 *   var X: Type := val  →  (assignment (varAssignDef (kVar) (identifier) (typeref (identifier))) (kAssign) (literalNumber))
 *   var X := val        →  (assignment (varAssignDef (kVar) (identifier)) (kAssign) (literalNumber))
 *   const C = val       →  (inlineConst (kConst) (identifier) (defaultValue (kEq) (literalNumber)))
 *   for var I := ...    →  (for (kFor) (assignment (varAssignDef ...)) (kTo) ... (kDo) body)
 */
function genInlineWithOracle(depth) {
    if (depth <= 0) {
        return { code: "G := 1;", expected: "(assignment (identifier) (kAssign) (literalNumber))" };
    }

    const choice = Math.floor(Math.random() * 5);

    if (choice === 0) { // Inline var with type
        const id = `V${Math.floor(Math.random() * 1000)}`;
        return {
            code: `var ${id}: Integer := 10;`,
            expected: `(assignment (varAssignDef (kVar) (identifier) (typeref (identifier))) (kAssign) (literalNumber))`
        };
    } else if (choice === 1) { // Inline var with inference
        const id = `V${Math.floor(Math.random() * 1000)}`;
        return {
            code: `var ${id} := 20;`,
            expected: `(assignment (varAssignDef (kVar) (identifier)) (kAssign) (literalNumber))`
        };
    } else if (choice === 2) { // Inline const
        const id = `C${Math.floor(Math.random() * 1000)}`;
        const val = Math.floor(Math.random() * 100);
        return {
            code: `const ${id} = ${val};`,
            expected: `(inlineConst (kConst) (identifier) (defaultValue (kEq) (literalNumber)))`
        };
    } else if (choice === 3) { // for-var loop
        const loopVar = `I${Math.floor(Math.random() * 100)}`;
        const limit = Math.floor(Math.random() * 20);
        const body = genInlineWithOracle(depth - 1);
        return {
            code: `for var ${loopVar} := 0 to ${limit} do ${body.code}`,
            expected: `(for (kFor) (assignment (varAssignDef (kVar) (identifier)) (kAssign) (literalNumber)) (kTo) (literalNumber) (kDo) ${body.expected})`
        };
    } else { // Inline var followed by usage
        const id = `V${Math.floor(Math.random() * 1000)}`;
        return {
            code: `var ${id} := G + 1;`,
            expected: `(assignment (varAssignDef (kVar) (identifier)) (kAssign) (exprBinary (identifier) (kAdd) (literalNumber)))`
        };
    }
}

function wrapExpected(innerExpected) {
    return `(root (defProc (declProc (kProcedure) (identifier)) (block (kBegin) ${innerExpected} (kEnd))))`;
}

// --- Main ---

ensureTmpDir();
const dcc32 = findDcc32();
validateToolchain(dcc32, tempFile, createDelphiProgram("var I := 10; for var J := 0 to 5 do G := I + J;"));

const { existingCode, maxTestId } = parseExistingCorpus(corpusFile);

const newResults = new Map();
let totalGenerated = 0;
let skippedExisting = 0;

console.log("2. Fuzzing Inline Vars/Consts with Oracle...");
for (let i = 0; i < 300; i++) {
    totalGenerated++;
    const oracle = genInlineWithOracle(Math.floor(Math.random() * 3) + 1);
    const fullCode = oracle.code;

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

const tempParseFile = `tmp/compare_inline_${process.pid}.pas`;

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

printReport('Inline Vars/Consts', {
    totalGenerated, skippedExisting,
    compiled: newResults.size,
    passCount, failCount, firstMismatch
});

if (fs.existsSync(tempParseFile)) fs.unlinkSync(tempParseFile);
cleanup(tempFile);
