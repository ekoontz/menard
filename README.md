[![Clojars Project](https://img.shields.io/clojars/v/babylon.svg)](https://clojars.org/babylon)
[![Build Status](https://secure.travis-ci.org/ekoontz/babylon.png?branch=master)](http://travis-ci.org/ekoontz/babylon)
[![License](https://img.shields.io/badge/License-EPL%201.0-red.svg)](https://opensource.org/licenses/EPL-1.0)

# Babylon

<div>
  <a href="https://en.wikipedia.org/wiki/Ishtar_Gate">
    <img alt="Image of Auroch from the Ishtar Gate of Babylon" 
         src="https://www.ancient.eu/uploads/images/738.jpg?v=1485682813" height="100">
  </a>
</div>

A Clojure library for generation and parsing of natural language expressions.

Based on a linguistic theory called HPSG (Head Driven Phrase Structure Grammar). More about HPSG:

- http://hpsg.stanford.edu/
- https://en.wikipedia.org/wiki/Head-driven_phrase_structure_grammar

# Getting Started

```
% lein repl
nREPL server started on port 51179 on host 127.0.0.1 - nrepl://127.0.0.1:51179
REPL-y 0.3.7, nREPL 0.2.12
Clojure 1.9.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_121-b13
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (load "babylon/english")
nil
user=> (in-ns 'babylon.english)
#namespace[babylon.english]
babylon.english=> (demo)
Generation:
===

= transitive sentences =

Groups see words.
Worlds see sheep.
Antonia sees dogs.
Works see sheep.
Schools see problems.
We see numbers.
Problems see people.
Cases see systems.
Books see jobs.
Students see you.

= reflexive sentences =

Eyes see themselves.
Works see themselves.
She sees herself.
He sees himself.
It sees itself.
Antonia sees herself.
Antonia sees herself.
She sees herself.
Antonio sees himself.
I see myself.

= 'long' sentences =

The programs that look go the mothers that use.
A political money feels the mothers that give.
An only program seems the questions countries help.
The dogs that I am do the question that tries.
The country eyes are leaves a government that looks.
The numbers that moneys begun want a name that stories seem.
A night Antonio play began a right that lots have.
The eyes parts talk live the tall points.
A dog that means say a man that helps.
The parts he are work an able point.

Parsing:
===

[s  .you *are], [s  .you *are], [s  .you *are], [s  .you *are], [s  .you *are], [s  .you *are], [s-slash  .you *are], [s-slash  .you *are], [s-slash  .you *are], [s-slash  .you *are], [s-slash  .you *are], [s-slash  .you *are]
[s  .dogs *make], [s-slash  .dogs *make]
[s  .she *is], [s  .she *is], [s  .she *is], [s-slash  .she *is], [s-slash  .she *is], [s-slash  .she *is]
[s  .questions *hear], [s-slash  .questions *hear]
[s  .stories *look], [s-slash  .stories *look]
WARN  15 feb 2019 08:35:54,496 babylon.parse: could not parse: "she began". Tokenizations attempted: she;began

[np  .a *world]
[s  .worlds *find], [s-slash  .worlds *find]
WARN  15 feb 2019 08:35:55,322 babylon.parse: could not parse: "he tries". Tokenizations attempted: he;tries

[np  .the *people]
10
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
