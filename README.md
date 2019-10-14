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
nREPL server started on port 60267 on host 127.0.0.1 - nrepl://127.0.0.1:60267
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
babylon.english         babylon.exception       babylon.generate        babylon.grammar         babylon.lexiconfn
babylon.morphology      babylon.parse           babylon.serialization   babylon.ug
user=> (babylon.english/demo)
Generation:
===

= transitive sentences =

Numbers would live her.
You would become students.
Mothers would move stories.
We would tell hands.
Ways would call names.
Groups would know words.
Mothers would live gentlemen.
Problems would keep points.
Numbers would help systems.
We would become people.

= reflexive sentences =

I would help myself.
She would say herself.
It would keep itself.
Hands would walk themselves.
I would take myself.
Antonia would tell herself.
She would ask herself.
Questions would feel themselves.
Problems would start themselves.
He would become himself.

= Interrogative sentences =

Have stories?
Did moneys need?
Have waters?
Have moneys?
Are rooms?
Have works?
Would times?
Have governments?
Have cases?
Are cases?

Parsing:
===

parses for: 'books worlds would teach': [nbar2 +books .[s-slash(:conditional) .worlds +[vp-aux-slash(:conditional) +would .teach]]], [nbar2 +books .[s-slash(:conditional) .worlds +[vp-aux-slash(:conditional) +would .teach]]]
parses for: 'facts schools would say': [nbar2 +facts .[s-slash(:conditional) .schools +[vp-aux-slash(:conditional) +would .say]]], [nbar2 +facts .[s-slash(:conditional) .schools +[vp-aux-slash(:conditional) +would .say]]]
parses for: 'things people would play': [nbar2 +things .[s-slash(:conditional) .people +[vp-aux-slash(:conditional) +would .play]]], [nbar2 +things .[s-slash(:conditional) .people +[vp-aux-slash(:conditional) +would .play]]]
parses for: 'works families would help': [nbar2 +works .[s-slash(:conditional) .families +[vp-aux-slash(:conditional) +would .help]]], [nbar2 +works .[s-slash(:conditional) .families +[vp-aux-slash(:conditional) +would .help]]]
parses for: 'lives what would seem': [nbar2 +lives .[s-slash(:conditional) .what +[vp-aux-slash(:conditional) +would .seem]]], [nbar2 +lives .[s-slash(:conditional) .what +[vp-aux-slash(:conditional) +would .seem]]]
parses for: 'hands schools would take': [nbar2 +hands .[s-slash(:conditional) .schools +[vp-aux-slash(:conditional) +would .take]]], [nbar2 +hands .[s-slash(:conditional) .schools +[vp-aux-slash(:conditional) +would .take]]]
parses for: 'numbers tables would think': [nbar2 +numbers .[s-slash(:conditional) .tables +[vp-aux-slash(:conditional) +would .think]]], [nbar2 +numbers .[s-slash(:conditional) .tables +[vp-aux-slash(:conditional) +would .think]]]
parses for: 'numbers would': [s(:conditional) .numbers +would]
parses for: 'they would': [s(:conditional) .they +would]
parses for: 'mothers numbers would be': [nbar2 +mothers .[s-slash(:conditional) .numbers +[vp-aux-slash(:conditional) +would .be]]], [nbar2 +mothers .[s-slash(:conditional) .numbers +[vp-aux-slash(:conditional) +would .be]]], [nbar2 +mothers .[s-slash(:conditional) .numbers +[vp-aux-slash(:conditional) +would .be]]], [nbar2 +mothers .[s-slash(:conditional) .numbers +[vp-aux-slash(:conditional) +would .be]]]
10
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
