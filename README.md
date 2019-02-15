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

years see her
companies see times
numbers see dogs
parts see puppies
rooms see women
people see companies
worlds see months
families see Antonio
hands see Antonia
hands see states

= reflexive sentences =

I see myself
I see myself
he sees himself
I see myself
it sees itself
Antonia sees herself
it sees itself
she sees herself
it sees itself
Antonio sees himself

= 'long' sentences =

the lot that is gives the month I am
the government that looks gets the strong times
the puppy that areas are is a small money
the weeks that things call became the year problems do
the waters that words take are the military names
the system that became talks a local money
the places that think help the ways that live
the hand months feel lives a better life
the systems that came move the true lots
a mother eyes work shows the lot that countries have

Parsing:
===

[s  .puppies *seem], [s-slash  .puppies *seem]
[s  .books *are], [s  .books *are], [s  .books *are], [s-slash  .books *are], [s-slash  .books *are], [s-slash  .books *are]
[s  .states *say], [s-slash  .states *say]
[np  .the *rights]
[np  .a *world]
[np  .the *family]
[s  .areas *look], [s-slash  .areas *look]
[np  .the *student]
[s  .hands *come], [s  .hands *come]
[np  .the *number]
10
babylon.english=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
