# Yet Another Lambda Calculus Interpreter
**Yet Another Lambda Calculus Interpreter**.

**YALCI** is a **Haskell** implementation of **System F** with a variety of **extensions**, **type checking** and **inference** (**Algorithm T**, as of yet). Initially made as what's described but only for **STLC**, **Simply Typed Lambda Calculus**.

## Instructions
The steps for setting **YALCI** up are as follows:

```cabal build```

```cabal run```

If warnings show up, fix them! In case you're required to install a **missing dependency**, it can be accomplished by using a command such as:

```cabal install --lib <library_name>```

The issue is very likely to be a missing dependency so this should suffice. **Cabal** usually fixes them by itself anyway.

**You need cabal and GHC in order to use YALCI, as you may have already noticed.**

Once you're running the program, write ```:?``` and press enter. It should give you about half the indications you need.

As for the other half, I suggest taking a look at **Benjamin Pierce's "Types and Programming Languages" book**, most particularly its **11th section** (which pertains to **Extensions**) as well all the way through the **22st** and the **24th** (which pertain to **Polymorphism**, including **Type Reconstruction**, **System F** and **Existential Quantifiers**).

## Syntax and Semantics

I'm going to show each construct and its meaning in a very informal manner.

| Syntax                | Semantic        | Type      |
|:---------------------:|:---------------|:---------|
| **true**                  | **true**<br>a primitive value      | **Bool** |
| **false**                 | **false**<br>a primitive value     | **Bool** |
| **if t1 then t2 else t3** | **if statement**<br>both possible outputs must be of the same type<br>and the condition has to be a boolean | **Bool**<br>where t1 has type Bool and T2 = T3<br>where type of t2 is T2 and type of t3 is T3 |
| **0**                    | **0**<br>a primitive value         | **Nat**  |
| **succ t**                | **successor of a natural**<br>used on naturals<br>otherwise there will be an error | **Nat**<br>where t has type Nat |
| **pred t**                | **predecessor of a natural**<br>used on naturals<br>otherwise there will be an error<br>using it on 0 converts itself into 0 | **Nat**<br>where t has type Nat |
| **iszero t**              | **checks if t is 0**<br>if so then converts into true else false | **Bool**<br>where t has type Bool |
| **x**                     | **variable**<br>it can be any alpha-numeric name<br>beginning with a lowercase letter<br> and ending with any number of primes (like myX')<br>this is only meant to have no abstraction in cases<br>where its type is being infered | **Type variable t**<br>it may have any type, depending on the abstraction |
| **\x:T.t**                     | **lambda abstraction\/function**<br>where x has to have type T (given by the programmer)<br>it will be type checked<br>it's treated as a value | **T -> T'**<br> where T' is the type of t |
| **\_:T.t**                     | **wildcard**<br>just like an abstraction but the bound variable does not occur in t, so it may be omitted | **T -> T'**<br>where T' is the type of t |
| **\x.t**                       | **lambda calculus abstraction without explicit typing**<br>its type will be inferred<br>which means t must include only this abs, app and var constructs | **T->T'**<br>where T is the inferred type variable of x<br>and T' the inferred type of t |
| **t1 t2**                      | **lambda calculus application**<br>it requires a space in between<br>first t1 evaluates, then t2 and then the app is evaluated | **T**<br> where T comes from t1's type: T' -> T,<br>and t2 must have type T' |
| **unit**                       | **unit**<br>a primitive value | **Unit** |
| **t1;t2**                      | **sequencing**<br>where t1 has to have type Unit<br>and it will be thrown away after evaluating | **T**<br>where T is the type of t2, and t1 must have type Unit |
| **t as T**                     | **ascription**<br>where T is a type given by the programmer and t's type must match it | **T**<br>where T is the type of t |
| **let x = t1 in t2**           | **let binding**<br>any occurrences of x in t2 are substituted by t1<br>but only after t1 is evaluated | **T**<br>where T is the type of t2 |
| **\{l1=t1, ...,ln=tn\}** | **record, which works like tuples**<br>but you can also give them labels for making projections<br>if a label is omitted, it will be as if it were label=n<br>where n is index of the cell where its in | **{l1:T1, ..., ln:Tn}**<br>where T'i is the type of t'i<br>(product types) |
| **let \{l1=p1, ..., ln=pn\} = \{l1=t1, ..., ln=tn\} in t** | **similar to the let case above**<br>**but with pattern matching for records**<br>the pattern matching (p'i) either contains another record-like pattern<br>or a variable name, and will assign it to it's ith counterpart | **T**<br>where T is the type of t<br>assuming the types of t1...tn |
| **\<l1=t1, ..., ln=tn\> as T** | **variant**<br>**works similarly to a data from haskell**<br>it's used for pattern matching using the case construct \<labels are required\> | **\<l1:T1, ..., ln:Tn\>**<>where T has the referred type<br>(sum types) |
| **case (\<li=vi\> as T) of <lj=xj> -> tj** | **the case includes a pattern**<br>**and it will match with the ith, out of all j's**<br>**which will then produce what's on the right side assuming xj=vi<br>also, each match is separated by a pipe (\|)** | **T**<br>where all tj must have the same the type T |
| **fix t** | **simulates recursion (fixed point)** | **T**<br>where t has type T -> T |
| **letrec x:T1 = t1 in t2** | **a recursive let binding**<br>which indirectly employs the fix construct | **T**<br>where t2 has type T assuming x has type T1 |
| **nil\[T\]**  | **empty list**<br>for elements of type T | **T** |
| **head\[T\] t** | **gets the head of a list of type T** | **T**<br>where t has type T |
| **tail\[T\] t** | **gets the tail of a list of type T** | **List T**<br>where t has type T |
| **isnil\[T\] t** | **checks if a list of type T is empty** | **Bool**<br>where t has type T |
| **cons\[T\] t1 t2** | **a list of type T**<br>with t1 as the head and t2 as the tail | **List T**<br>where t1 has type T and t2 has type List T |
| **X** | **type variable or base type**<br>it must begin with an uppercase letter | 
| **\X.t** | **type abstraction**<br>abstracts a type using a type variable<br>it's treated as a value | **∀X.T**<br>where t has type T<br>and X may occur in T |
| **t\[T\]** | **type application**<br>allows the type T to replace<br>all occurrences of a certain variable within T<br>this targets type annotations | **\[X\-\>T\]T'**<br>where t1 has type ∀X.T' |
| **\{\*T, t\} as \{∃X, T'\}** | **packs t**<br>this will allow its type to<br>be stay hidden and only be<br>compatible with terms from t | **\{∃X, T\}**<br>where T'' is the type of t<br>and T' = \[X\-\>T\] |
| **let \{X, x\} = t1 in t2** | **Unpacks t1 into t2**<br>this allows t1 to be used withint t2<br>just as explained for the **pack** constructor | **T2**<br>where T2 is the type of t2<br>assuming the existence of X<br>and t1 must have type T1 = {∃X, T1'}<br>and, at last, assuming x has type T1' |

Don't forget to take a look at the ```programs/default_tests.txt``` file, as it contains examples for all constructors of the language.

Writing a number **n** is the same as writing succ(succ(...(0)...) (so, **n** succ's). The parser handles this. If the result of a program is in its entirety a number, then it will be pretty-printed as such, rather than the succession of succ's.

You may write programs directly into the command line, as long as they neither follow nor are followed by anything else. Press enter and **YALCI** will echo its interpretation of the program, its type and its normal form (what it evaluates to, after being evaluated as much as possible).

There are a couple of synonyms for the **Lambda Symbol**, the **Arrow Symbol**, the **Universal Quantifier Symbol** and the **Existential Quantifier Symbol**:
  - **Lambda Symbol: \, λ, Λ, lambda;**
  - **Arrow Symbol: \-\>, →;**
  - **Universal Quantifier Symbol: All, forall, ∀;**
  - **Existential Quantifier Symbol: Some, forsome, ∃.**

The existential types and packing/unpacking operations allow functional style **OOP** (**Object-Oriented Programming**) as well as the usage of **ADT's** (**Abstract Data Types**). Take a look at the **Existential Quantifiers** section (the **24th**) from **Pierce's** book in case you're curious as to how each of the two can be accomplished.

## Types

There are **three primitive types**: **Bool**, **Nat** and **Unit**. **Bool** is for **booleans**, **Nat** is for **natural numbers** and **Unit** is for **unit** (with no real application besides sequencing, as of now). Then there are **Type Variables**, which are only used for **Type Inference**; they are displayed as a "t" followed by some number starting from 1. Try writing the term \x.x in the command-line. Another form of type **Type Variables** is the one used by System F. Check the **Syntax and Semantics** table for more information.

{l1:T1, ..., ln:Tn} is related to the **Product Type**, more particularly it regards **records**.

\<l1:T1, ..., ln:Tn\> is related to the **Sum Type**, more particularly it regards **variants**.

T->T' is the **Type Arrow**, it comes from **abstractions**.

List T is the **List Type** where the elements of a **list** are all of type T.

∀X.T is the **Quantifier Type**, which represents a term where its outermost abstraction is a type abstraction. T may contain X's. Comes from **type abstractions**.

\{∃X, T\} is the **Existential Type**, which represents a term where an abstracted X occurs in T. Looking at the type alone can be tricky, so check the table for the **Syntax and Semantics**. It comes from **packs**.

X is the **Base Type**, which must begin with an uppercase letter and may contain in between numbers or letters and end with any number of primes. There's no real use to these other than them serving as a placeholder for **Type Abstractions**. It comes from unbound **type variables**.

## Notes

**Existentials** can be encoded within **System F** alone, but I didn't end up implementing the **desugaring** for that. I didn't feel like it was worth it, my time is better used elsewhere. **Pierce** talked about it in his 24th section (it has a rather hint-y designation), so you can see how it's done in there.

The interpreter uses **De Bruijn** representation of terms in order to facilitate shifting and substituting operations. **Alpha Conversion** can be assumed for apparently conflicting substitutions. If two bound names conflict, the innermost name will be given an extra prime until there are not more conflicts.

Simply **Typed Lambda Calculus** is well-known, but **System F** tends to stay in the shadows. **System F** is a far more expressive calculus, known as **Polymorphic Lambda Calculus**. The idea is that types can be quantified, not just terms. For a good reason, it corresponds to **Second Order Logic** via the **Curry-Howard Correspondence**, and this **polymorphism** is known as **Impredicative Polymorphism**. Its inference has been shown to be undecideable for quite a while by Wells \[1994\]. In any case, it's used by various compilers, even the most modern ones - which goes to show how impactful **System F** has been to the world of **Programming Languages**.

Regarding **Type Inference** or **Type Reconstruction**, **YALCI** includes both **Algorithm T** and **Algorithm W** (**Hindley-Milner-Damas** style). Unfortunately, these algorithms only cover the least amount of constructors needed for their basic functioning.

## Commands

Most of the commands are simple and similar. The table is dense because there are multiple configurations for the same thing. It was not well thought out, but serves its purpose. I hope this was not too much of a hurdle.

| Command(s) | Usage | Description |
|------------|-------|------------|
| *(All commands)* | — | Command names (the first token of the command) are not case sensitive. |
| :var, :v, :assign, :a | :v \<var_name\> | Assign a written term to <var_name>. |
| :type, :ty, :t | :t \<var_name\> | Show the type of the term assigned to <var_name>. |
| :eval, :ev, :e | :e \<var_name\> | Fully evaluate the term from <var_name>. |
| :evaln, :evn, :en | :en <number_of_steps> <var_name> | Evaluate (<number_of_steps>) n-steps the term from <var_name>. |
| :help, :h, :? | :h | Display information regarding the commands. |
| :show, :sh, :s | :s \<var_name\> | Show the term assigned to <var_name>. |
| :desugar, :desug, :des, :d | :d <var_name1> <var_name2> | Desugar the term from <var_name1> and place it into <var_name2>. |
| :var, :v, :assign, :a, (+ eval) | :v <var_name1> :ev <var_name2> | Evaluate from the current environment (given <var_name2>) and store into <var_name1>. |
| :var, :v, :assign, :a, (+ evaln) | :v <var_name1> :evn <number_of_steps> <var_name2> | Evaluate n-steps from the current environment and store into <var_name1>. |
| :load, :l | :l \<file_path\> | Load terms from file at <file_path>, assigned as `<var_name> := <expression>`, and load into the environment. |
| :v?, :vars | :v? | Show the first page (10 environment variables) if a number is not specified. |
| :v?, :vars | :v? \<number\> | Show the <number>'th page (containing 10 environment variables' names). |
| :m, :mv, :move | :mv \<var_name1\> \<var_name2\> | Store the contents of <var_name2> into <var_name1>. |
| :q, :quit | :q | Close the REPL. |
| :te, :tenv, :testenv | :testenv | Attempt to type all variables in the environment. |
| :ee, :eenv, :evalenv | :evalenv | Attempt to evaluate all variables in the environment. |
| :c, :ce, :cenv, :clear, :clearenv | :c | Clear the environment (no variables accessible until new ones are added). |
| :de, :denv, :desenv, :desugenv, :desugarenv | :de | Desugar all variables in the environment. |
| :av?, :allvars | :av? | Show all variables in the environment. |
| :showenv, :showe, :senv, :se | :se | Show the environment. |
| :showenv, :showe, :senv, :se | :se \<page_number\> | Show a specific environment page. |
| :te, :tenv, :testenv | :te \<page_number\> | Type a specific environment page. |
| :ee, :eenv, :evalenv | :ee \<page_number\> | Evaluate a specific environment page. |
| <program> | <program> | Shows, then Types and then Evaluates the given program/term. |
| *(Environment pages)* | — | Page numbers start at 1. |

## Lastly...

Don't forget to **report** any **bugs**, **mistakes** or **flaws**! **Do criticize!** **Thank you!**
