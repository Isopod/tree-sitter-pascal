// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "TreeSitterPascal",
    platforms: [.macOS(.v10_13), .iOS(.v11)],
    products: [
        .library(name: "TreeSitterPascal", targets: ["TreeSitterPascal"]),
    ],
    dependencies: [],
    targets: [
        .target(name: "TreeSitterPascal",
                path: ".",
                exclude: [
                    "../../bindings/rust/Cargo.toml",
                    "../../bindings/c/Makefile",
                    "../../bindings/node/binding.gyp",
                    "../../bindings/c",
                    "../../bindings/go",
                    "../../bindings/node",
                    "../../bindings/python",
                    "../../bindings/rust",
                    "../../prebuilds",
                    "../../grammar.js",
                    "../../bindings/node/package.json",
                    "../../bindings/node/package-lock.json",
                    "../../bindings/python/pyproject.toml",
                    "../../bindings/python/setup.py",
                    "../../test",
                    "../../examples",
                    "../../.editorconfig",
                    "../../.github",
                    "../../.gitignore",
                    "../../.gitattributes",
                    "../../.gitmodules",
                ],
                sources: [
                    "../../src/parser.c",
                    // NOTE: if your language has an external scanner, add it here.
                ],
                resources: [
                    .copy("../../queries")
                ],
                publicHeadersPath: ".",
                cSettings: [.headerSearchPath("../../src")])
    ],
    cLanguageStandard: .c11
)
