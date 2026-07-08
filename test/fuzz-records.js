const fs = require('fs');
const { execSync } = require('child_process');
const {
    findDcc32, ensureTmpDir, validateToolchain,
    parseExistingCorpus, appendToCorpus, cleanParserOutput,
    cleanup
} = require('./fuzz-shared-code');

const tempFile = 'tmp/fuzz_records_temp.pas';
const corpusFile = 'test/corpus/diabolical-records.txt';
const TEST_PREFIX = 'Fuzzed Records';

function createDelphiProgram(recordBody) {
    // Extract operator names to generate implementations
    const ops = [];
    if (recordBody.includes('Initialize')) ops.push('class operator TMyRecord.Initialize(out Dest: TMyRecord); begin end;');
    if (recordBody.includes('Finalize')) ops.push('class operator TMyRecord.Finalize(var Dest: TMyRecord); begin end;');
    if (recordBody.includes('Assign')) ops.push('class operator TMyRecord.Assign(var Dest: TMyRecord; const [ref] Src: TMyRecord); begin end;');

    return `
program FuzzRecords;
{$APPTYPE CONSOLE}
type
  TMyRecord = record
    ${recordBody}
  end;

${ops.join('\n')}

begin
end.
`;
}

/**
 * The Oracle for Custom Managed Records (Initialize, Finalize, Assign).
 */
function genManagedRecordWithOracle() {
    const ops = [
        {
            code: 'class operator Initialize(out Dest: TMyRecord);',
            expected: '(declProc (kClass) (kOperator) (identifier) (declArgs (declArg (kOut) (identifier) (type (typeref (identifier))))))'
        },
        {
            code: 'class operator Finalize(var Dest: TMyRecord);',
            expected: '(declProc (kClass) (kOperator) (identifier) (declArgs (declArg (kVar) (identifier) (type (typeref (identifier))))))'
        },
        {
            code: 'class operator Assign(var Dest: TMyRecord; const [ref] Src: TMyRecord);',
            expected: '(declProc (kClass) (kOperator) (identifier) (declArgs (declArg (kVar) (identifier) (type (typeref (identifier)))) (declArg (kConst) (identifier) (type (typeref (identifier))))))'
            // NOTE: the expected AST for [ref] is speculative. If it fails, it will reveal how [ref] is actually parsed (or if it's an error).
        }
    ];

    const count = Math.floor(Math.random() * 3) + 1;
    const selected = [];
    const used = new Set();
    while (selected.length < count) {
        const idx = Math.floor(Math.random() * ops.length);
        if (!used.has(idx)) {
            selected.push(ops[idx]);
            used.add(idx);
        }
    }

    const code = selected.map(s => s.code).join('\n    ');
    // For expected, we need to wrap each one.
    const expected = selected.map(s => s.expected).join(' ');

    return { code, expected };
}

function wrapExpected(innerExpected) {
    return `(root (declTypes (kType) (declType (identifier) (kEq) (declClass (kRecord) ${innerExpected} (kEnd)))))`;
}

// --- Main ---

ensureTmpDir();
const dcc32 = findDcc32();
validateToolchain(dcc32, tempFile, createDelphiProgram("class operator Initialize(out Dest: TMyRecord);"));

const { existingCode, maxTestId } = parseExistingCorpus(corpusFile);

const validTests = [];
let totalGenerated = 0;
let skippedExisting = 0;

console.log("2. Generating and compiling Managed Record permutations...");
for (let i = 0; i < 50; i++) {
    totalGenerated++;
    const oracle = genManagedRecordWithOracle();
    const fullCode = oracle.code;

    if (existingCode.has(fullCode)) { skippedExisting++; continue; }

    const programStr = createDelphiProgram(fullCode);
    fs.writeFileSync(tempFile, programStr);

    try {
        execSync(`${dcc32} -B -CC -W- -H- ${tempFile}`, { stdio: 'ignore' });
        validTests.push({ code: fullCode, expected: oracle.expected });
        process.stdout.write('+');
    } catch (err) {
        process.stdout.write('.');
    }
}

console.log(`\n\nGenerated: ${totalGenerated}`);
console.log(`Already in corpus: ${skippedExisting}`);
console.log(`New compiled: ${validTests.length}`);

console.log("\n3. Parsing with tree-sitter...");
let errorFreeCount = 0;
let errorCount = 0;
const newTests = [];
let testId = maxTestId;

const tempParseFile = `tmp/compare_records_${process.pid}.pas`;

for (const { code, expected } of validTests) {
    const testCode = `type TMyRecord = record\n  ${code.replace(/\n/g, '\n  ')}\nend;`;
    fs.writeFileSync(tempParseFile, testCode + '\n');

    const fullExpected = wrapExpected(expected).replace(/\s+/g, ' ').trim();

    try {
        const output = execSync(`npx tree-sitter parse ${tempParseFile}`, { encoding: 'utf8' }).trim();
        const actual = cleanParserOutput(output);

        if (actual.includes('ERROR') || actual.includes('MISSING') || actual !== fullExpected) {
            // It's a failure or a mismatch! We DEFINITELY want this in the corpus.
            errorCount++;
            testId++;
            newTests.push({
                name: `${TEST_PREFIX} ${testId}`,
                code: testCode,
                expected: fullExpected // Use the ORACLE'S expectation, not the broken output
            });
            process.stdout.write('F'); // F for Failure added to corpus
        } else {
            errorFreeCount++;
            testId++;
            newTests.push({
                name: `${TEST_PREFIX} ${testId}`,
                code: testCode,
                expected: fullExpected
            });
            process.stdout.write('.');
        }
    } catch (e) {
        // Even if the command crashes, we want the test
        errorCount++;
        testId++;
        newTests.push({
            name: `${TEST_PREFIX} ${testId}`,
            code: testCode,
            expected: fullExpected
        });
        process.stdout.write('X');
    }
}

appendToCorpus(corpusFile, newTests);

console.log(`\n\nDIABOLICAL REPORT: Managed Records`);
console.log("=================");
console.log(`Compiled (Valid): ${validTests.length}`);
console.log(`Parsed clean:    ${errorFreeCount}`);
console.log(`Parse errors:    ${errorCount}`);

if (errorCount > 0) {
    console.log(`\n[RESULT] Found ${errorCount} cases where valid Managed Records fail to parse.`);
}

if (fs.existsSync(tempParseFile)) fs.unlinkSync(tempParseFile);
cleanup(tempFile);
