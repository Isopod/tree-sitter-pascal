import ctypes
from unittest import TestCase

import tree_sitter
import tree_sitter_pascal


class TestLanguage(TestCase):
    def test_returns_language_capsule(self):
        capsule = tree_sitter_pascal.language()
        self.assertEqual(type(capsule).__name__, "PyCapsule")

        get_name = ctypes.pythonapi.PyCapsule_GetName
        get_name.restype = ctypes.c_char_p
        get_name.argtypes = [ctypes.py_object]

        self.assertEqual(get_name(capsule).decode(), "tree_sitter.Language")

    def test_can_load_grammar(self):
        try:
            tree_sitter.Language(tree_sitter_pascal.language())
        except Exception:
            self.fail("Error loading Pascal grammar")
