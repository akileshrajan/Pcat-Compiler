# Pcat-Compiler
**The whole project is built on JAVA-Scala stack

phase-1: Lexical Analysis using JFLex scanner generator(java).
Phase-2: Paser usin CUP parser genarator(JAVA).
Phase-3: AST's adding semantics to the parser(Scala).
phase-4: Typechecking on AST's (scala)
phase-5: Generating IR for AST's (scala){code generation}

Phase-6: Instruction selection i.e. generating MIPS code using SIPM  

test/*.pcat : contains all the test cases files to test through compilers.

pcat.lex : Lexical analysis code,

pcat.cup : Grammar definations, using RE defined by PCAT mannual-{google it}

Typecheck.scala : Typechecking the AST.

AST.scala : generation of ASTs
