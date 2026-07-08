const fs = require('fs');
const { execSync } = require('child_process');
const {
    findDcc32, ensureTmpDir, validateToolchain,
    parseExistingCorpus, appendToCorpus, cleanParserOutput,
    cleanup
} = require('./fuzz-shared-code');

const tempFile = 'tmp/fuzz_preprocessor_temp.pas';
const corpusFile = 'test/corpus/diabolical-preprocessor.txt';
const TEST_PREFIX = 'Fuzzed Preprocessor';

function createDelphiProgram(body) {
    return `
program FuzzPreprocessor;
{$APPTYPE CONSOLE}
var
  A: Integer;
begin
  ${body}
end.
`;
}

function genSplitPreprocessor(depth) {
    if (depth <= 0) {
        return "begin A := 1; end;";
    }

    const type = Math.random() < 0.5 ? 'ifdef' : 'if';
    const cond = type === 'ifdef' ? 'FOO' : '1=1';

    // Split between var and begin
    return `
{$${type} ${cond}}
  var V_${depth}: Integer;
begin
  A := ${depth};
{$else}
  var V_${depth}_alt: Double;
begin
  A := ${depth} + 1;
{$endif}
end;`;
}

/**
 * Generates nested preprocessor directives.
 */
function genNestedPreprocessor(depth) {
    if (depth <= 0) {
        return "A := 1;";
    }

    const type = Math.random() < 0.5 ? 'ifdef' : 'if';
    const cond = type === 'ifdef' ? 'FOO' : '1=1';
    const inner = genNestedPreprocessor(depth - 1);

    if (Math.random() < 0.3) {
        return `{$${type} ${cond}}\n  ${inner}\n{$else}\n  ${inner}\n{$endif}`;
    } else {
        return `{$${type} ${cond}}\n  ${inner}\n{$endif}`;
    }
}

// --- Main ---

ensureTmpDir();
const dcc32 = findDcc32();
validateToolchain(dcc32, tempFile, createDelphiProgram("{$ifdef A}{$ifdef B}A:=1;{$endif}{$endif}"));

const { existingCode, maxTestId } = parseExistingCorpus(corpusFile);

const validTests = [];
let totalGenerated = 0;
let skippedExisting = 0;

console.log("2. Generating and compiling Preprocessor permutations...");
for (let i = 0; i < 60; i++) {
    totalGenerated++;
    let code;
    if (i < 30) {
        code = genNestedPreprocessor(Math.floor(Math.random() * 3) + 1);
    } else {
        // These will be used in a different context
        code = genSplitPreprocessor(Math.floor(Math.random() * 2) + 1);
    }

    if (existingCode.has(code)) { skippedExisting++; continue; }

    const programStr = (i < 30) ? createDelphiProgram(code) : `program FuzzPP; var A: Integer; procedure Test; ${code} begin end.`;
    fs.writeFileSync(tempFile, programStr);

    try {
        execSync(`${dcc32} -B -CC -W- -H- ${tempFile}`, { stdio: 'ignore' });
        validTests.push({ code, isSplit: i >= 30 });
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

const tempParseFile = `tmp/compare_pp_${process.pid}.pas`;

for (const { code, isSplit } of validTests) {
    const testCode = isSplit 
        ? `procedure Test;\n${code.replace(/\n/g, '\n')}`
        : `procedure Test;\nbegin\n  ${code.replace(/\n/g, '\n  ')}\nend;`;
    fs.writeFileSync(tempParseFile, testCode + '\n');

    try {
        const output = execSync(`npx tree-sitter parse ${tempParseFile}`, { encoding: 'utf8' }).trim();
        const cleaned = cleanParserOutput(output);

        errorFreeCount++;
        testId++;
        newTests.push({
            name: `${TEST_PREFIX} ${testId}`,
            code: testCode,
            expected: cleaned
        });
        
        if (cleaned.includes('ERROR') || cleaned.includes('MISSING')) {
            errorCount++;
            process.stdout.write('F');
        } else {
            process.stdout.write('.');
        }
    } catch (e) {
        errorCount++;
        process.stdout.write('X');
    }
}

appendToCorpus(corpusFile, newTests);

console.log(`\n\nDIABOLICAL REPORT: Preprocessor`);
console.log("=================");
console.log(`Compiled (Valid): ${validTests.length}`);
console.log(`Parsed clean:    ${errorFreeCount}`);
console.log(`Parse errors:    ${errorCount}`);

if (errorCount > 0) {
    console.log(`\n[RESULT] Found ${errorCount} cases where nested preprocessor fails to parse correctly.`);
}

if (fs.existsSync(tempParseFile)) fs.unlinkSync(tempParseFile);
cleanup(tempFile);
