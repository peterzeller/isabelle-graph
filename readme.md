# Isabelle Dependency Graph generator

Tries to build a dependency graph for Isabelle.

- Works on text only, so does not support implicit dependencies like `[simp]` attributes.
- At the moment the parser is very simple, it only collects identifiers and definitions used in the program and builds the graph from them.

## Usage:

    sbt run <folder>