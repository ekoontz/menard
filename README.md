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
nREPL server started on port 61915 on host 127.0.0.1 - nrepl://127.0.0.1:61915
REPL-y 0.4.3, nREPL 0.6.0
Clojure 1.10.1
OpenJDK 64-Bit Server VM 12.0.1+12
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (load "babylon/english")
nil
user=> (babylon.
babylon.english         babylon.exception       babylon.generate        babylon.grammar         babylon.lexiconfn       babylon.morphology
babylon.parse           babylon.serialization   babylon.ug
user=> (babylon.english/demo)
Generation:
===

= transitive sentences =

Groups would play Antonia.
Areas would run jobs.
Men would turn Antonia.
We would be mothers.
Nights would run countries.
Girls would live women.
Men would be women.
Things would take Antonio.
Times would talk to.
It would show rooms.

= reflexive sentences =

She would be herself.
Books would have themselves.
It would show itself.
I would turn myself.
It would live itself.
I would find myself.
He would walk himself.
It would take itself.
She would turn herself.
It would like itself.

= Interrogative sentences =

Do points come?
Were questions?
Would waters?
Are problems?
Does who feel?
Have ways?
Were problems?
Have mothers?
Are words?
Are numbers?

= 'WH' interogative sentences =

Who did numbers talk to?
To what did ways talk?
Who do weeks go to?
What do moneys go to?
What does what look at?
To what do jobs go?
On what do women put?
Who do homes look at?
To who do waters go?
Who did lives talk to?

Parsing:
===

parses for: 'eyes would': [s(:conditional) .eyes +would]
parses for: 'cats would': [s(:conditional) .cats +would]
parses for: 'puppies programs would do': [nbar2 +puppies .[s-slash(:conditional) .programs +[vp-aux-slash(:conditional) +would .do]]], [nbar2 +puppies .[s-slash(:conditional) .programs +[vp-aux-slash(:conditional) +would .do]]]
parses for: 'rooms would': [s(:conditional) .rooms +would]
parses for: 'mothers I would be': [nbar2 +mothers .[s-slash(:conditional) .I +[vp-aux-slash(:conditional) +would .be]]], [nbar2 +mothers .[s-slash(:conditional) .I +[vp-aux-slash(:conditional) +would .be]]], [nbar2 +mothers .[s-slash(:conditional) .I +[vp-aux-slash(:conditional) +would .be]]], [nbar2 +mothers .[s-slash(:conditional) .I +[vp-aux-slash(:conditional) +would .be]]]
parses for: 'years businesses would be': [nbar2 +years .[s-slash(:conditional) .businesses +[vp-aux-slash(:conditional) +would .be]]], [nbar2 +years .[s-slash(:conditional) .businesses +[vp-aux-slash(:conditional) +would .be]]], [nbar2 +years .[s-slash(:conditional) .businesses +[vp-aux-slash(:conditional) +would .be]]], [nbar2 +years .[s-slash(:conditional) .businesses +[vp-aux-slash(:conditional) +would .be]]]
parses for: 'schools problems would take': [nbar2 +schools .[s-slash(:conditional) .problems +[vp-aux-slash(:conditional) +would .take]]], [nbar2 +schools .[s-slash(:conditional) .problems +[vp-aux-slash(:conditional) +would .take]]]
parses for: 'parts would': [s(:conditional) .parts +would]
parses for: 'businesses what would eat': [nbar2 +businesses .[s-slash(:conditional) .what +[vp-aux-slash(:conditional) +would .eat]]], [nbar2 +businesses .[s-slash(:conditional) .what +[vp-aux-slash(:conditional) +would .eat]]]
parses for: 'companies women would say': [nbar2 +companies .[s-slash(:conditional) .women +[vp-aux-slash(:conditional) +would .say]]], [nbar2 +companies .[s-slash(:conditional) .women +[vp-aux-slash(:conditional) +would .say]]]
10
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
