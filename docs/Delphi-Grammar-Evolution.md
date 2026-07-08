Because Delphi has maintained strict backward compatibility while absorbing three decades of modern programming paradigms, its grammar is highly contextual. This guide details the strict difference between reserved words and directives, and a comprehensive timeline of when specific syntax rules were introduced or modified.

### **1. Lexical Grammar: Tokens, Reserved Words, and Directives**

At the lexical level, a Delphi program is parsed into tokens separated by whitespace or comments (which can be enclosed in {}, (**), or //)

A critical challenge when building a Delphi grammar is distinguishing between **Reserved Words** and **Directives**:

- **Reserved Words:** These keywords (e.g., class, interface, begin, end, try, if) have fixed meanings and can never be used as user-defined identifiers
- **Directives:** These are contextual keywords (e.g., read, write, absolute, dynamic, strict). They trigger specific compiler behaviors when placed in designated syntactic locations—like inside a class or property declaration—but the parser must allow them to be used as standard variables or method names elsewhere in the code

### **2. Syntax Evolution: Version-by-Version Grammar Changes**

To accurately parse specific versions of Delphi, your grammar must support the following syntactic additions based on their release milestones:

**Delphi 4**

- **Arrays:** Introduced dynamic array syntax (e.g., array of Integer).
- **Interface Delegation:** Introduced the implements directive. Grammar rule: property <name>: <interface> read <field> implements <interface>; 

**Delphi 2005 & 2006**

- **Loops:** Introduced the for <element> in <collection> do iteration syntax
- **Class Architecture:** Added the class helper for syntax
- **Visibility:** Introduced the strict private and strict protected scoping directives within class declarations
- **Overloading:** Added syntax for operator overloading within records

**Delphi 2009**

- **Generics:** A massive grammar update introducing parameterized types. Grammar rule: type <ClassName><<TypeParam>> = class.
- **Anonymous Methods:** Introduced closures. Grammar rule: reference to procedure/function and the inline declaration procedure (x: Integer) begin... end; without an identifier

**Delphi 2010**

- **Attributes:** Introduced metaprogramming annotations. Grammar rule: [<AttributeName>(<OptionalParams>)] placed immediately preceding a type, field, or method declaration

**Delphi 10.3 Rio**

- **Inline Variables:** Fundamentally altered traditional Pascal block structures. Variable declarations (var) are no longer restricted to the header of a routine; they can be declared inline within begin...end execution blocks
- **Type Inference:** Added the := initialization at the point of declaration (e.g., var I := 22;), allowing the parser to infer the type automatically

**Delphi 10.4 Sydney**

- **Custom Managed Records:** Added syntax to bind memory lifecycle operations to records. Grammar rule: class operator Initialize(out Dest: T); and class operator Finalize(var Dest: T);

**Delphi 11 Alexandria**

- **Lexical Numbers:** Updated the numeric literal parser to allow the underscore (_) as a digit separator for readability (e.g., 1_000_000). Underscores cannot appear at the beginning of the literal

**Delphi 12 Athens**

- **Multiline Strings:** Updated the string literal parser. Multiline strings are now enclosed by triple quotes (''') and can span across physical line breaks in the source code

**Delphi 13 Florence**

- **Ternary Operator:** Transformed the if keyword from a pure statement into an inline expression operator. Grammar rule: <Variable> := if <Condition> then <Expr> else <Expr>;
- **Logical Operators:** Added is not and not in as unified comparative operators.
- **Directives:** Added the noreturn procedure directive to indicate a routine permanently interrupts execution flow.

### **3. Expression Grammar and Operator Precedence**

When mapping the mathematical and logical rules of the language, your EBNF production rules must accurately reflect Delphi's strict operator precedence hierarchy to avoid ambiguity

1. **Highest Precedence (Unary):** @, not, ^
2. **Second Precedence (Multiplicative/Bitwise):** *, /, div, mod, and, shl, shr, as
3. **Third Precedence (Additive/Logical):** +, -, or, xor
4. **Lowest Precedence (Relational):** =, <>, <, >, <=, >=, in, is, is not, not in

*Grammar implementation note:* The introduction of the ternary conditional operator in Delphi 13 requires careful parser integration. Because + and - have higher precedence than the ternary if, an expression like if A then B else C + D is evaluated by the compiler as if A then B else (C + D) . If a developer intends to concatenate the result of the entire conditional evaluation, the grammar enforces the use of parentheses: `(if A then B else C) + D` .