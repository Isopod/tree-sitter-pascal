import XCTest
import SwiftTreeSitter
import TreeSitterPascal

final class TreeSitterPascalTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_pascal())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Pascal grammar")
    }
}
