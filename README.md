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
Warning: implicit hook found: lein-environ.plugin/hooks
Hooks are deprecated and will be removed in a future version.
nREPL server started on port 61788 on host 127.0.0.1 - nrepl://127.0.0.1:61788
REPL-y 0.4.3, nREPL 0.6.0
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

Businesses have turned systems.
He had had groups.
Rights will keep dogs.
Lives have sheep.
Rooms ate mothers.
We had known businesses.
She goes to women.
Hands were doing things.
Programs would see jobs.
You had let girls.

= reflexive sentences =

She would leave herself.
It will know itself.
I will begin myself.
I let myself.
She will do herself.
It was working itself.
Antonia had called herself.
Antonia will use herself.
It was going to itself.
It will go to itself.

= Interrogative sentences =

Will she let?
Has it used?
Had numbers tried?
Have women lived?
Will lots teach?
Are puppies keeping?
Would numbers see?
Had names kept?
Had schools taught?
Would men be?

Parsing:
===

[s  .weeks *[vp-aux  *are .walking]], [s-slash  .weeks *[vp-aux-slash  *are .walking]], [s-slash  .weeks *[vp-aux-slash  *are .walking]]
[np  .the *works]
[np  .the *person]
[s  .facts *move], [s-comp  .facts *move], [s-slash  .facts *move]
[np  .the *book]
[np  .the *ways]
[np  .a *case]
[np  .a *way]
[np  .the *point]
[np  .the *stories]
10
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
