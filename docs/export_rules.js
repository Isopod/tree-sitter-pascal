const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

const grammarPath = path.join(__dirname, '..', 'grammar.js');
const corpusPath = path.join(__dirname, '..', 'test', 'corpus');
const outputPath = path.join(__dirname, 'rules.md');

function getCategory(ruleName) {
    if (ruleName.startsWith('_')) return 'Internal Helpers';
    if (ruleName.startsWith('k')) return 'Keywords & Terminals';
    if (['root', 'program', 'unit', 'library', 'interface', 'implementation', 'initialization', 'finalization', 'moduleName'].includes(ruleName)) return 'High-Level Structure';
    if (['if', 'while', 'repeat', 'for', 'foreach', 'try', 'case', 'assignment', 'block', 'asm', 'with', 'raise', 'goto', 'label', 'inlineConst', 'varDef', 'varAssignDef', 'statement', 'caseCase', 'caseLabel', 'exceptionHandler', 'exceptionElse', 'statements', 'statementsTr'].includes(ruleName)) return 'Statements';
    if (ruleName.startsWith('expr') || ruleName === 'range') return 'Expressions';
    if (ruleName.startsWith('decl') || ruleName.startsWith('def') || ruleName.startsWith('typeref') || ruleName.startsWith('generic') || ruleName === 'type' || ruleName === 'guid' || ruleName === 'rttiAttributes' || ruleName === 'procAttribute' || ruleName === 'procExternal' || ruleName === 'operatorName' || ruleName === 'operatorDot') return 'Declarations & Definitions';
    if (ruleName.startsWith('literal') || ruleName === 'recInitializer' || ruleName === 'arrInitializer' || ruleName === 'recInitializerField') return 'Literals';
    return 'Other';
}

function getSlug(name) {
    return name.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-+|-+$/g, '');
}

function exportRules() {
    const grammarContent = fs.readFileSync(grammarPath, 'utf8');
    const rules = new Set();
    
    // 1. Extract rule names
    const ruleRegex = /^\s*([a-zA-Z0-9_]+):\s*\$ =>/gm;
    let match;
    while ((match = ruleRegex.exec(grammarContent)) !== null) {
        rules.add(match[1]);
    }
    const dynamicRuleRegex = /\[rn\('([a-zA-Z0-9_]+)'\)/g;
    while ((match = dynamicRuleRegex.exec(grammarContent)) !== null) {
        rules.add(match[1]);
    }

    // 2. Scan corpus files for usage and map rules to specific test cases
    const corpusFiles = fs.readdirSync(corpusPath).filter(f => f.endsWith('.txt'));
    const ruleTests = {}; // rule -> Set(unique test identifiers)
    const allTestInfos = {}; // unique test identifier -> { name, file, pass: bool }
    
    rules.forEach(rule => { ruleTests[rule] = new Set(); });

    let totalBlocksFound = 0;
    corpusFiles.forEach(file => {
        const content = fs.readFileSync(path.join(corpusPath, file), 'utf8');
        const lines = content.split(/\r?\n/);
        
        let i = 0;
        while (i < lines.length) {
            if (lines[i].trim() === '===') {
                i++;
                if (i >= lines.length) break;
                
                const testName = lines[i].trim().replace(/\r$/, "");
                i++;
                if (i >= lines.length || lines[i].trim() !== '===') continue;
                i++;
                
                let codeAndAST = "";
                while (i < lines.length && lines[i].trim() !== '===') {
                    codeAndAST += lines[i] + "\n";
                    i++;
                }
                
                totalBlocksFound++;
                const testId = `${file}::${testName}`;
                allTestInfos[testId] = { name: testName, file: file, pass: true };
                
                const parts = codeAndAST.split(/\r?\n---\r?\n/);
                const astPart = parts[1] || "";
                
                rules.forEach(rule => {
                    const searchPattern = new RegExp(`\\(${rule}\\b`, 'g');
                    if (searchPattern.test(astPart)) {
                        ruleTests[rule].add(testId);
                    }
                });
            } else {
                i++;
            }
        }
    });

    // 3. Run tree-sitter test and parse results to update pass/fail status
    // Parse overview output to mark failing tests
    const fileStats = {}; // Keep for the file table
    const failedTests = new Set();

    console.log(`Scanning ${corpusFiles.length} corpus files for failures...`);

    corpusFiles.forEach(f => {
        const base = f.replace('.txt', '');
        fileStats[f] = { pass: 0, fail: 0 };
        
        let testOutput = "";
        try {
            // Filter by filename to get ALL tests for this file, bypassing global failure limits
            const cmd = process.platform === 'win32' 
                ? `chcp 65001 > nul && npx tree-sitter test -f ${base}` 
                : `npx tree-sitter test -f ${base}`;
            testOutput = execSync(cmd, { encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'] });
        } catch (e) {
            testOutput = e.stdout ? e.stdout.toString() : (e.stderr ? e.stderr.toString() : "");
        }

        // Strip ANSI and normalize
        testOutput = testOutput.replace(/\x1b\[[0-9;]*m/g, "");

        // 3a. Find failures in the summary list for this specific file
        const failuresSectionMatch = testOutput.match(/\d+ failures?:\s*\n\n([\s\S]*?)$/);
        if (failuresSectionMatch) {
            const failuresContent = failuresSectionMatch[1];
            const failureLines = failuresContent.split('\n');
            failureLines.forEach(line => {
                const m = line.match(/^\s+\d+\.\s+(.*?):/);
                if (m) failedTests.add(m[1].trim());
            });
        }

        // 3b. Match sections in overview
        const sectionRegex = new RegExp(`^\\s{2}${base}:`, 'm');
        const sectionMatch = testOutput.match(sectionRegex);
        
        if (sectionMatch) {
            const startIndex = sectionMatch.index;
            const nextSectionRegex = /\n  [\w-]+:|\n\d+ failure/;
            const sectionEndMatch = testOutput.slice(startIndex + 1).match(nextSectionRegex);
            const endIndex = sectionEndMatch ? startIndex + 1 + sectionEndMatch.index : testOutput.length;
            const section = testOutput.slice(startIndex, endIndex);
            
            const testLines = section.split('\n').filter(line => /^\s+\d+\.\s+\S/.test(line));
            testLines.forEach(line => {
                const nameMatch = line.match(/\d+\.\s+\S+\s+(.*)$/);
                const testName = nameMatch ? nameMatch[1].trim().replace(/\r/g, "") : "";
                const testId = `${f}::${testName}`;
                
                const isFail = failedTests.has(testName) || line.includes('Γ£ù') || line.includes('✗') || line.includes('\u2717');
                const isPass = !isFail;
                
                if (allTestInfos[testId]) {
                    allTestInfos[testId].pass = isPass;
                    if (!isPass) {
                        console.log(`DEBUG: Found FAILURE for [${testId}]`);
                    }
                }
                
                if (isPass) fileStats[f].pass++;
                else fileStats[f].fail++;
            });
        }
    });

    // 4. Organize by category
    const categories = {};
    rules.forEach(rule => {
        const cat = getCategory(rule);
        if (!categories[cat]) categories[cat] = [];
        categories[cat].push(rule);
    });

    // 5. Calculate Summary & Totals
    const catStats = {};
    const totals = { rules: 0, tested: 0, untested: 0, totalTests: 0, pass: 0, fail: 0 };
    
    // Global totals from all unique tests
    const uniqueTests = Object.values(allTestInfos);
    uniqueTests.forEach(t => {
        if (t.pass) totals.pass++; else totals.fail++;
        totals.totalTests++;
    });

    Object.keys(categories).sort().forEach(catName => {
        const catRules = categories[catName];
        let testedCount = 0;
        const catUniqueTests = new Set();

        catRules.forEach(rule => {
            if (ruleTests[rule].size > 0) testedCount++;
            ruleTests[rule].forEach(tid => catUniqueTests.add(tid));
        });

        let catPass = 0;
        let catFail = 0;
        catUniqueTests.forEach(tid => {
            if (allTestInfos[tid].pass) catPass++; else catFail++;
        });

        catStats[catName] = {
            rules: catRules.length,
            tested: testedCount,
            untested: catRules.length - testedCount,
            totalTests: catUniqueTests.size,
            pass: catPass,
            fail: catFail
        };

        totals.rules += catStats[catName].rules;
        totals.tested += catStats[catName].tested;
        totals.untested += catStats[catName].untested;
    });

    // 6. Generate Markdown for rules.md
    let summaryTable = '| Category | Rules | Tested | Untested | Total Tests | Passing | Failing |\n';
    summaryTable += '| :--- | :---: | :---: | :---: | :---: | :---: | :---: |\n';
    
    Object.keys(catStats).sort().forEach(cat => {
        const s = catStats[cat];
        summaryTable += `| [${cat}](#${getSlug(cat)}) | ${s.rules} | ${s.tested} | ${s.untested} | ${s.totalTests} | ${s.pass} | ${s.fail} |\n`;
    });
    
    summaryTable += `| **TOTAL** | **${totals.rules}** | **${totals.tested}** | **${totals.untested}** | **${totals.totalTests}** | **${totals.pass}** | **${totals.fail}** |\n`;
    
    let output = '# Tree-sitter Delphi Rules List\n\n';
    output += '## <a id="summary"></a>Summary\n\n';
    output += summaryTable;
    output += '\n---\n\n';

    let fileTable = '## <a id="file-status"></a>Test File Status\n\n';
    fileTable += '| File | Tests | Passing | Failing |\n';
    fileTable += '| :--- | :---: | :---: | :---: |\n';
    Object.keys(fileStats).sort().forEach(file => {
        const s = fileStats[file];
        const total = s.pass + s.fail;
        fileTable += `| \`${file}\` | ${total} | ${s.pass} | ${s.fail} |\n`;
    });
    output += fileTable;
    output += '\n---\n\n';

    Object.keys(categories).sort().forEach(cat => {
        output += `## <a id="${getSlug(cat)}"></a>${cat}\n\n`;
        output += '| Rule Name | Tested In |\n';
        output += '| :--- | :--- |\n';
        
        categories[cat].sort().forEach(rule => {
            const tests = ruleTests[rule].size > 0 
                ? Array.from(ruleTests[rule]).map(tid => `\`${allTestInfos[tid].file}\``)
                : [];
            const uniqueTestFiles = Array.from(new Set(tests)).join(', ');
            output += `| **${rule}** | ${uniqueTestFiles || '*No explicit test found*'} |\n`;
        });
        output += '\n[Back to Summary](#summary)\n\n';
    });

    fs.writeFileSync(outputPath, output);
    console.log(`Successfully exported rules to ${outputPath}`);
    formatMarkdown(outputPath);

    // 7. Update CONTRIBUTING.md
    const contributingPath = path.join(__dirname, '..', 'CONTRIBUTING.md');
    if (fs.existsSync(contributingPath)) {
        let contributingContent = fs.readFileSync(contributingPath, 'utf8');
        // Replace links in summaryTable for CONTRIBUTING.md (point to docs/rules.md)
        let contributingSummary = summaryTable.replace(/\]\(#/g, '](docs/rules.md#');

        const startMarker = '<!-- TEST_SUMMARY_START -->';
        const endMarker = '<!-- TEST_SUMMARY_END -->';
        const startIndex = contributingContent.indexOf(startMarker);
        const endIndex = contributingContent.indexOf(endMarker);

        if (startIndex !== -1 && endIndex !== -1) {
            contributingContent = contributingContent.substring(0, startIndex + startMarker.length)
                + '\n\n' + contributingSummary + '\n'
                + contributingContent.substring(endIndex);
            fs.writeFileSync(contributingPath, contributingContent);
            console.log(`Successfully updated summary in CONTRIBUTING.md`);
            formatMarkdown(contributingPath);
        }
    }
}

function formatMarkdown(filePath) {
    try {
        execSync(`npx prettier --write "${filePath}"`, { stdio: 'inherit' });
        execSync(`npx markdownlint-cli2 --fix --config .markdownlint.jsonc "${filePath}"`, { stdio: 'inherit' });
    } catch (e) {
        console.warn(`Warning: Could not format ${filePath}. Ensure prettier and markdownlint-cli2 are installed.`);
    }
}

exportRules();
