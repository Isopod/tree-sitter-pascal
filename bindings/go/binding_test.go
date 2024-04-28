package tree_sitter_pascal_test

import (
	"testing"

	tree_sitter "github.com/smacker/go-tree-sitter"
	"github.com/tree-sitter/tree-sitter-pascal"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_pascal.Language())
	if language == nil {
		t.Errorf("Error loading Pascal grammar")
	}
}
