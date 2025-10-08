# Rustell

Rustell is a development tool for Rust, designed for users who are not accustomed to using the semicolon (";") as a statement separator - for example, developers coming from languages like Haskell.

Its usage is similar to "rustfmt": Rustell reads Rust source code (which may omit some required semicolons) from standard input ("stdin") and produces corrected output with all necessary semicolons inserted through standard output ("stdout"). It is intended to be used as a text editor plugin, typically invoked right before running "rustfmt".

Rustell is implemented as a partial Rust parser. It parses only those parts of the code that depend on semicolons, such as "use", "mod", and "let" statements, as well as statements inside "{ ... }" code blocks (excluding the final statement in a block). Rustell converts these expressions into an internal AST, consuming any existing semicolons if present. Code that does not belong to these statements is left untouched and represented as a special "raw" node type within the AST.

Finally, Rustell renders its AST back into Rust source code, appending semicolons where required while preserving "raw" nodes exactly as they appeared in the original input.
