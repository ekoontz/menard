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
nREPL server started on port 52766 on host 127.0.0.1 - nrepl://127.0.0.1:52766
REPL-y 0.3.7, nREPL 0.2.12
Clojure 1.10.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_121-b13
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (load "babylon/english")
nil
user=> (babylon.english/demo)
Generation:
===

= transitive sentences =

Antonia saw hands.
Systems see questions.
Places saw Antonio.
Moneys see schools.
Months see eyes.
She saw students.
Times see facts.
Worlds see jobs.
Companies see areas.
Weeks saw studies.

= reflexive sentences =

It saw itself.
It saw itself.
Puppies saw themselves.
I saw myself.
It sees itself.
It sees itself.
Schools saw themselves.
Antonio saw himself.
He saw himself.
I see myself.

= 'long' sentences =

The waters that began think the able questions.
The days sheep use took the old story.
The lots that knew take a state that jobs need.
The waters that began walk the sheep that works talk.
The eyes that women move tell the businesses that ran.
A problem that shows kept the waters that did.
The months that like left the only days.
The times cases are find an eye that kept.
The whole cats feel a fact systems try.
The jobs that rooms put found the rooms that put.

Parsing:
===

[np  .a *money]
[s  .questions *help], [s-slash  .questions *help]
[s  .groups *take], [s-slash  .groups *take]
[s  .worlds *have], [s-slash  .worlds *have]
[np  .a *right]
[s  .waters *began]
[np  .the *studies]
[np  .a *life]
[s  .lives *come]
[np  .a *company]
10
babylon.english=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
