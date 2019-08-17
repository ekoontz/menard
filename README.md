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
nREPL server started on port 52895 on host 127.0.0.1 - nrepl://127.0.0.1:52895
REPL-y 0.4.3, nREPL 0.6.0
Clojure 1.10.0
OpenJDK 64-Bit Server VM 12.0.1+12
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

It can call.
We meant parts.
Rooms have kept governments.
We have made men.
Rooms had eaten days.
Stories would seem rights.
Places must want.
Homes are seeing months.
Jobs were putting words.
Ways will look at girls.

= reflexive sentences =

Ways will go to themselves.
Stories were talking to themselves.
Antonia was putting herself.
She has turned herself.
Antonia is teaching herself.
He has let himself.
It will find itself.
He is using himself.
It was saying itself.
I will put myself.

= Interrogative sentences =

He runs?
Asked myself?
Sheep have had?
Will tell?
Have meant?
It takes?
Work students?
We mean?
Have groups?
Jobs would turn?

Parsing:
===

[np .the *day]
[s(:past) .people *showed], [s-comp .people *showed], [s-slash(:past) .people *showed]
[s(:past) .she *was], [s(:past-progressive) .she *was], [s-slash(:past) .she *was], [s-slash(:past) .she *was]
WARN  17 Aug 2019 13:14:06,564 babylon.lexiconfn: (matching-lexemes 'put'): both regular inflections (18) and exceptions (6).
[nbar3 *studies .[comp1 *that .put]], [nbar3 *studies .[comp1 *that .put]], [nbar3 *studies .[comp1 *that .put]], [nbar3 *studies .[comp1 *that .put]], [nbar3 *studies .[comp1 *that .put]], [nbar3 *studies .[comp1 *that .put]], [nbar3 *studies .[comp1 *that .put]], [nbar3 *studies .[comp1 *that .put]]
[s-interog *are .girls], [vp(:present-simple) *are .girls]
[nbar3 *states .[comp1 *that .began]], [nbar3 *states .[comp1 *that .began]]
[s(:present-simple) .Antonia *has], [s(:perfect) .Antonia *has], [s-slash(:present-simple) .Antonia *has]
[s(:present-simple) .lives *move], [s-comp .lives *move], [s-slash(:present-simple) .lives *move], [s-slash(:modal) .lives *move]
[s(:past) .[nbar2 *facts .[s-slash(:past) .lots *had]] *had], [s(:pluperfect) .[nbar2 *facts .[s-slash(:past) .lots *had]] *had], [s-comp .[nbar2 *facts .[s-slash(:past) .lots *had]] *had], [s-comp .[nbar2 *facts .[s-slash(:past) .lots *had]] *had], [s-comp .[nbar2 *facts .[s-slash(:past) .lots *had]] *had], [s-slash(:past) .[nbar2 *facts .[s-slash(:past) .lots *had]] *had]
WARN  17 Aug 2019 13:14:14,862 babylon.lexiconfn: (matching-lexemes 'have'): both regular inflections (24) and exceptions (18).
[s(:present-simple) .dogs *have], [s(:present-simple) .dogs *have], [s(:perfect) .dogs *have], [s-comp .dogs *have], [s-comp .dogs *have], [s-comp .dogs *have], [s-slash(:present-simple) .dogs *have], [s-slash(:present-simple) .dogs *have], [s-slash(:modal) .dogs *have]
10
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
