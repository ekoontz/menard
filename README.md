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
nREPL server started on port 60806 on host 127.0.0.1 - nrepl://127.0.0.1:60806
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
user=> (babylon.english/demo)
Who do bikes mean?
Works believe studies would.
Who do rooms go to?
At what do words look?
He says that hands would.
Parts want to believe girls.
Cases showed the houses that kept.
The old table would make a federal family.
The black group likes lots that the international kittens would teach.
Who did students become?
Countries say questions would.
Who do problems look at?
At what did waters look?
Books believe that Antonia would.
She wants to keep cats.
Cases have the rooms that ask.
The sure thing would mean the young nights.
The military people begin questions that a national girl would become.
What do you hear?
Who do tables go to?
Places believe moneys would.
At what did schools look?
Rights believe that schools would.
Parts want to run puppies.
It says the programs that saw.
The old bike would know the little fact.
The special problem becomes boys that a social kitten would tell.
Who did programs have?
What do works look at?
I hope moneys would.
At what did waters look?
Houses say that waters would.
Works want to call states.
A whole family would begin the young names.
People hear a student that talks.
The right puppies hear groups that the national company would use.
Who did eyes move?
What do boys talk to?
Jobs believe lots would.
At who did programs look?
Countries say that gentlemen would.
The big bikes would walk the early question.
Mothers take a company that knew.
Problems want to think that groups would.
The free question walks years that the low house would teach.
(9 9 9 9 9)
user=> (map babylon.english/syntax-tree (babylon.english/parse "problems want to think that groups would"))
WARN  03 Nov 2019 10:44:09,308 babylon.parse: overh caught unification failure: [s-slash(:conditional) ._ +_]; head: would
...
("[s(:modal-present) .problems +[vp-modal-1(:modal-present) +want .[infinitive +to .[vp(:base) +think .[comp2 +that .[s(:conditional) .groups +would]]]]]]" "[s(:modal-present) .problems +[vp-modal-1(:modal-present) +want .[infinitive +to .[vp(:base) +think .[comp2 +that .[s(:conditional) .groups +would]]]]]]" "[s-comp .problems +[vp-modal-1(:base) +want .[infinitive +to .[vp(:base) +think .[comp2 +that .[s(:conditional) .groups +would]]]]]]" "[s-comp .problems +[vp-modal-1(:base) +want .[infinitive +to .[vp(:base) +think .[comp2 +that .[s(:conditional) .groups +would]]]]]]")
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
