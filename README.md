# CodeFormatter

Our project aims to format and lint Python files in Haskell. It is split between 4 modules: PythonAST, Parser, Formatter, and Linter, each with a corresponding tester. Supporting new syntax simply requires extending the AST, implementing it to the parser, adding any edge cases to the formatter and specify any common errors for the linter. Then add tests to each of the testing suites.

src/formatter.config hosts parameters for user defined specifications to the outputted file - there's currently a bug so I hardcode 4 as the tabWidth in some cases.
src/PythonAST.hs is a representation of a Python AST as it's own data type and expressions possible in Python
src/PythonFormatter.hs formats the file, cleaning indentation, fixing incorrect indentation, and cleaning up spaces between operators and delimiters.
src/PythonLinter.hs returns static analysis about the code, currently warning about unused imports and recognizing camelCase and providing the snake_case equivalent.
src/PythonParser.hs uses the AST to parse a file. Given the AST being able to represent all grammar, it would deprecate some of the functionality PythonFormatter.hs on completion.

This is a stack project that utilizes Parsec and HUnit.

> stack build

should give every required module and

> stack run {source of python file}

should return a cleaned file. 

Submission Notes
Jack Sherick

4. I struggled learning with the scope of a proper formatter, since only an AST will be able to properly cover every single case, as depth and scope no longer bear an issue. Block depth was a struggle, and so I was happy when I was able to figure out how to properly integrate both user indentation and Python rules to have a program that returns both logically sound code, but also functionally what the user inputted. The formatter can both use the state and the current indentation to reason out a line ahead of the state, so outside of grievous user error it should work. Many an hour was spent on figuring that out, and while scope-wise it's far less than the AST, it gives a reasonably, logical place to present from since it's able to handle very ugly code and. 

5. Integration of my code with the AST would be able to cover some block depth issues; over-indentation is easy, but un-indentation can be difficult, and while the formatter currently will be reasonably accurate in most real world cases, the AST provides a way for far more cases to be correctly assumed. Between proglang looming over this submission and a simple lack of comfort in both the tree structure and Haskell, I was only able to accomplish what PythonFormatter.hs does currently.

6. I think a big issue for me wasn't Haskell, but Python. Handling de-indentation was something that took me forever - on the first presentation I was able to do most of what we showed on the second, and the code was always syntaxially correct, but, it would sometimes incorrectly indent code because at any given point the programmer can de-indent to move out of a block. Unlike other languages that provide a syntactic way of moving out of a block (semicolons), Python's indentation works perfectly if we assume the user to indent perfectly. Naturally, this project exists because we don't code perfectly, and so that determination brought me so much grief when, if this were formatting Java, could use the semicolons to perfectly know how to indent each line, never needing to use the indentation of the code itself, instead being able to know purely based off the last and current line of code. 

If I were to not use Haskell, I would probably use Python, just because of the available libraries and support the language has. But pattern matching and type safety saved countless hours, and that's before how easy it is to use the State Monad. Definitely for this project, Haskell is the most appropriate language I (now) know, since for how simple the code is, it handles a lot of cases well, I can only imagine the Hell of not having pattern matching for edge cases and how terrible the code would look, even if implementation remains the same. 