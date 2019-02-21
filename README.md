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
nREPL server started on port 65218 on host 127.0.0.1 - nrepl://127.0.0.1:65218
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
babylon.english=> (babylon.english/demo)
Generation:
===

= transitive sentences =

Eyes see governments.
We see families.
Points see things.
Words see me.
Days see systems.
Weeks see me.
Homes see programs.
Questions see hands.
Lots see mothers.
Books see systems.

= reflexive sentences =

He sees himself.
He sees himself.
Problems see themselves.
Antonio sees himself.
I see myself.
It sees itself.
It sees itself.
Antonio sees himself.
I see myself.
I see myself.

= 'long' sentences =

The bad businesses let a thing dogs turn.
The places that puppies move use a puppy studies make.
The things women let give the certain names.
A world that Antonio gets means a question lots need.
The works that hands give let a state that waters help.
The major businesses play the cases that we get.
The students that companies run turn a problem that eyes find.
The points that companies began make the night governments get.
The true lots begin the men that began.
A book that says lets the facts that days want.

Parsing:
===

[np  .a *program]
[s  .rooms *take], [s-slash  .rooms *take]
[np  .the *water]
[s  .waters *work], [s-slash  .waters *work]
[s  .you *became], [s-slash  .you *became]
[np  .a *system]
[np  .the *parts]
[s  .points *go]
[np  .a *mother]
[np  .the *things]
10
babylon.english=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
