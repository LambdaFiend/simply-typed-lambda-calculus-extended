# Revision history for STLCE

## 0.1.0.0 -- 2026-02-10

* First version. Named STLCE. It includes a lexer/parser and pretty printing, with its core being simply typed lambda calculus with extensions, typechecking, evaluation and desugaring for most constructors.

## 0.1.1.0 -- 2026-02-16

* Second version. Still named STLCE. It includes all previous features with the addition of a somewhat capable REPL, type inference using algorithm T and W, depending on the necessity (checking vs infering with T vs infering with W). Added default tests in case the user needs help understanding the capabilities of STLCE.

## 0.2.0.0 -- 2026-02-28

* Third version. Renamed to YALCI. It includes all previous features with the addition of System F and existential types which can be encoded using System F (despite that, it was not implemented due to it requiring the dev to work unecessaraly; not that it was that difficult, you can check its encoding in B. Pierce's book TAPL). Many improvements (a.k.a., generally, bug fixes) in general. Added even more tests cases, mostly from Pierce's own tests.
