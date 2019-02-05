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

Antonio sees Antonio
I see myself
women see questions
Antonio sees Antonio
works see Antonia
Antonia sees herself
she sees herself
groups see ways
places see her
Antonia sees Antonia

= reflexive sentences =

women see themselves
Antonia sees Antonia
Antonio sees himself
Antonio sees Antonio
it sees itself
I see myself
Antonia sees herself
he sees himself
Antonia sees Antonia
Antonio sees himself

= 'long' sentences =

the small moneys see the tall words
the tall weeks see the tall facts
the tall word sees a tall question
a tall man sees the small businesses
a tall name sees the small governments
the small week sees the tall work
the tall puppies see the tall jobs
the tall governments see a small money
the small cases see the small works
the small rooms see the tall book

Parsing:
===

[np  .the *groups]
[np  .the *stories]
[np  .a *thing]
[s  .days *sleep]
[s  .books *sleep]
[s  .studies *see]
[np  .the *life]
[np  .a *mother]
[s  .cats *sleep]
[s  .problems *see]
10
babylon.english=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
