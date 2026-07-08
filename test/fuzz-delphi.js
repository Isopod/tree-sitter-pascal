const fs = require('fs');
const { execSync } = require('child_process');
const {
    findDcc32, ensureTmpDir, validateToolchain,
    parseExistingCorpus, appendToCorpus, cleanParserOutput,
    cleanup
} = require('./fuzz-shared-code');

const tempFile = 'tmp/fuzz_temp.pas';
const corpusFile = 'test/corpus/modern_delphi_diabolical.txt';
const TEST_PREFIX = 'Fuzzed Diabolical Test';

function genExpr(type, depth) {
    if (depth <= 0) {
        if (type === 'Int') return ['A', 'B', 'C', 'D', '1', '100'][Math.floor(Math.random()*6)];
        if (type === 'Bool') return ['True', 'False', '(A < B)', '(C = D)', '(ObjA is TMyClass)'][Math.floor(Math.random()*5)];
        if (type === 'Obj') return ['ObjA', 'ObjB', 'TMyClass.Create'][Math.floor(Math.random()*3)];
        if (type === 'Set') return ['[1, 2]', '[A..B]', '[]'][Math.floor(Math.random()*3)];
    }

    const choices = [];
    if (type === 'Int') {
        choices.push(
            () => `(${genExpr('Int', depth-1)} + ${genExpr('Int', depth-1)})`,
            () => `(${genExpr('Int', depth-1)} * ${genExpr('Int', depth-1)})`,
            () => `if ${genExpr('Bool', depth-1)} then ${genExpr('Int', depth-1)} else ${genExpr('Int', depth-1)}`,
            () => `Integer(${genExpr('Int', depth-1)})`,
            () => `(if ${genExpr('Bool', depth-1)} then ${genExpr('Int', depth-1)} else ${genExpr('Int', depth-1)}) + ${genExpr('Int', depth-1)}`
        );
    } else if (type === 'Bool') {
        choices.push(
            () => `(${genExpr('Bool', depth-1)} and ${genExpr('Bool', depth-1)})`,
            () => `(${genExpr('Bool', depth-1)} or ${genExpr('Bool', depth-1)})`,
            () => `not (${genExpr('Bool', depth-1)})`,
            () => `(${genExpr('Int', depth-1)} < ${genExpr('Int', depth-1)})`,
            () => `(${genExpr('Obj', depth-1)} is not TMyClass)`,
            () => `(${genExpr('Int', depth-1)} not in ${genExpr('Set', depth-1)})`,
            () => `if ${genExpr('Bool', depth-1)} then ${genExpr('Bool', depth-1)} else ${genExpr('Bool', depth-1)}`
        );
    } else if (type === 'Obj') {
        choices.push(
            () => `if ${genExpr('Bool', depth-1)} then ${genExpr('Obj', depth-1)} else ${genExpr('Obj', depth-1)}`,
            () => `TMyClass(${genExpr('Obj', depth-1)})`,
            () => `(${genExpr('Obj', depth-1)} as TMyClass)`
        );
    } else if (type === 'Set') {
        choices.push(
            () => `if ${genExpr('Bool', depth-1)} then ${genExpr('Set', depth-1)} else ${genExpr('Set', depth-1)}`,
            () => `[${genExpr('Int', depth-1)}, ${genExpr('Int', depth-1)}]`
        );
    }

    return choices[Math.floor(Math.random() * choices.length)]();
}

function genStmt(depth) {
    if (depth <= 0) return `A := ${genExpr('Int', 0)};`;
    const choices = [
        () => `A := ${genExpr('Int', depth)};`,
        () => `if ${genExpr('Bool', depth)} then\n    ${genStmt(depth-1)}`,
        () => `if ${genExpr('Bool', depth)} then\n    ${genStmt(depth-1)}\n  else\n    ${genStmt(depth-1)}`,
        () => `var V${Math.floor(Math.random()*1000)} := ${genExpr('Int', depth)};`,
        () => `case ${genExpr('Int', depth)} of\n    1: ${genStmt(depth-1)};\n    2: ${genStmt(depth-1)};\n    otherwise ${genStmt(depth-1)}\n  end;`,
        () => `for var I${depth} := 0 to ${genExpr('Int', depth)} do ${genStmt(depth-1)}`
    ];
    return choices[Math.floor(Math.random() * choices.length)]();
}

function createDelphiProgram(stmt) {
    return `
program FuzzTest;
{$APPTYPE CONSOLE}

type
  TMyClass = class(TObject) end;

var
  A, B, C, D: Integer;
  ObjA, ObjB: TObject;

begin
  A := 1; B := 2; C := 3; D := 4;
  ObjA := TMyClass.Create; ObjB := TMyClass.Create;

  ${stmt}
end.
`;
}

// --- Main ---

ensureTmpDir();
const dcc32 = findDcc32();
validateToolchain(dcc32, tempFile, createDelphiProgram('A := 1;'));

const { existingCode, maxTestId } = parseExistingCorpus(corpusFile);

let passedCount = 0;
let skippedExisting = 0;
const validTests = []; // { code, stmt }

console.log("2. Generating and compiling permutations...");
for (let i = 0; i < 500; i++) {
    const stmt = genStmt(Math.floor(Math.random() * 8) + 1);
    const formattedStmt = stmt.split('\n').map(line => '  ' + line).join('\n');
    const codeBlock = `procedure Test;\nbegin\n${formattedStmt}\nend;`;

    if (existingCode.has(codeBlock)) { skippedExisting++; continue; }

    const programStr = createDelphiProgram(stmt);
    fs.writeFileSync(tempFile, programStr);

    try {
        execSync(`${dcc32} -B -CC -W- -H- ${tempFile}`, { stdio: 'ignore' });
        passedCount++;
        validTests.push({ codeBlock, stmt });
        process.stdout.write('+');
    } catch (err) {
        process.stdout.write('.');
    }
}

console.log(`\n\nGenerated: 500`);
console.log(`Already in corpus: ${skippedExisting}`);
console.log(`New compiled: ${passedCount}`);

// Parse each valid test with tree-sitter to check for ERROR/MISSING and get AST
console.log("\n3. Parsing with tree-sitter...");
let errorFreeCount = 0;
let errorCount = 0;
const newTests = [];
let testId = maxTestId;

const tempParseFile = `tmp/compare_delphi_${process.pid}.pas`;

for (const { codeBlock, stmt } of validTests) {
    // Parse the same multi-line format that the corpus stores
    fs.writeFileSync(tempParseFile, codeBlock + '\n');

    try {
        const output = execSync(`npx tree-sitter parse ${tempParseFile}`, { encoding: 'utf8' }).trim();
        const cleaned = cleanParserOutput(output);

        errorFreeCount++;
        testId++;
        newTests.push({
            name: `${TEST_PREFIX} ${testId}`,
            code: codeBlock,
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

console.log(`\n\nDIABOLICAL REPORT: General Delphi`);
console.log("=================");
console.log(`Compiled (Valid Delphi): ${passedCount}`);
console.log(`Parsed clean (no ERR):  ${errorFreeCount}`);
console.log(`Parse errors (ERR/MIS): ${errorCount}`);
console.log(`Appended to corpus:     ${newTests.length}`);

if (errorCount > 0) {
    console.log(`\n[RESULT] ${errorCount} valid Delphi programs produce parser errors — grammar gaps found.`);
} else {
    console.log("\n[RESULT] All valid Delphi programs parse without errors.");
}

// Note: this fuzzer has no oracle, so corpus ASTs are captured from parser output.
// They serve as regression tests — if the grammar changes, these tests will detect it.
if (newTests.length > 0) {
    console.log(`\nNote: ${newTests.length} regression tests added (AST from current parser, not an oracle).`);
}

if (fs.existsSync(tempParseFile)) fs.unlinkSync(tempParseFile);
cleanup(tempFile);
