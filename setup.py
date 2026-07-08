from os.path import isdir, join
from platform import system

from setuptools import Extension, setup
from setuptools.command.bdist_wheel import bdist_wheel
from setuptools.command.build_py import build_py


class BuildPy(build_py):
    def run(self):
        super().run()
        if isdir("queries"):
            dest = join(self.build_lib, "tree_sitter_pascal", "queries")
            self.copy_tree("queries", dest)


class BdistWheel(bdist_wheel):
    def get_tag(self):
        python, abi, platform = super().get_tag()
        if python.startswith("cp"):
            python, abi = "cp38", "abi3"
        return python, abi, platform


setup(
    ext_package="tree_sitter_pascal",
    ext_modules=[
        Extension(
            name="_binding",
            sources=[
                "bindings/python/tree_sitter_pascal/binding.c",
                "src/parser.c",
            ],
            extra_compile_args=["-std=c11"] if system() != "Windows" else [],
            define_macros=[
                ("Py_LIMITED_API", "0x03080000"),
                ("PY_SSIZE_T_CLEAN", None),
            ],
            include_dirs=["src"],
            py_limited_api=True,
        )
    ],
    cmdclass={
        "build_py": BuildPy,
        "bdist_wheel": BdistWheel,
    },
)
