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

## Acknowledgements

Based on a linguistic theory called HPSG (Head Driven Phrase Structure Grammar). More about HPSG:

- http://hpsg.stanford.edu/
- https://en.wikipedia.org/wiki/Head-driven_phrase_structure_grammar

Dutch grammar and lexicon based on [Carole Fehringer, "A Reference Grammar of Dutch", Cambridge University Press, 1999](https://books.google.nl/books/about/A_Reference_Grammar_of_Dutch.html?id=hXZNkFqILp0C&redir_esc=y). 

## Demo

For the demo, a sentence is generated for each specification listed in
<a href="https://github.com/ekoontz/babylon/blob/master/src/babylon/english/expressions.edn">expressions.edn</a>. 
Each expression is printed out first in its surface form, then in its the syntax tree form, as shown in the output below:

```
% lein repl
nREPL server started on port 56360 on host 127.0.0.1 - nrepl://127.0.0.1:56360
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
Who did moneys take?
[s-wh-interog .who +[s-interog-slash +did .[s-comp-2 .moneys +take]]]

false
user=> I think works would.
[s(:present-simple) .I +[vp(:present-simple) +think .[s(:present-simple) .works +would]]]

Who did cats go to?
[s-wh-interog .who +[s-interog-slash +did .[s-comp-2 .cats +[vp-slash +go .to]]]]

The very strong areas become me.
[s(:present-simple) .[np(:present-simple) .the +[nbar(:present-simple) .[intensifier(:present-simple) +very .strong] +areas]] +[vp(:present-simple) +become .me]]

Areas would not put.
[s(:conditional) .areas +[vp-aux(:conditional) +would .[negp(:conditional) +not .put]]]

At what did tables look?
[s-wh-interog .[pp +at .what] +[s-interog-slash +did .[s-comp-2 .tables +look]]]

Do places need to be?
[s-interog +do .[s-comp .places +[vp-modal-1 +need .[infinitive +to .be]]]]

Moneys believe that bikes would.
[s(:present-simple) .moneys +[vp(:present-simple) +believe .[comp2(:present-simple) +that .[s(:present-simple) .bikes +would]]]]

Jobs were not putting.
[s(:past-progressive) .jobs +[vp-aux(:past-progressive) +were .[negp(:past-progressive) +not .putting]]]

Bikes move the studies that felt.
[s(:present-simple) .bikes +[vp(:present-simple) +move .[np(:present-simple) .the +[nbar3(:present-simple) +studies .[comp1(:present-simple) +that .felt]]]]]

Cats were not turning you.
[s(:past-progressive) .cats +[vp-aux(:past-progressive) +were .[negp(:past-progressive) +not .[vp(:past-progressive) +turning .you]]]]

An extremely economic business would run the bad place.
[s(:conditional) .[np(:conditional) .a +[nbar(:conditional) .[intensifier(:conditional) +extremely .economic] +business]] +[vp-aux(:conditional) +would .[vp(:conditional) +run .[np(:conditional) .the +[nbar(:conditional) .bad +place]]]]]

The very best gentlemen sit on a company.
[s(:modal) .[np(:modal) .the +[nbar(:modal) .[intensifier(:modal) +very .best] +gentlemen]] +[vp-adjunct(:modal) +sit .[pp(:modal) +on .[np(:modal) .a +company]]]]

Rooms want to believe that parts would.
[s(:modal-present) .rooms +[vp-modal-1(:modal-present) +want .[infinitive(:modal-present) +to .[vp(:modal-present) +believe .[comp2(:modal-present) +that .[s(:modal-present) .parts +would]]]]]]

An extremely true business tells cases that the extremely recent country would work.
[s(:present-simple) .[np(:present-simple) .a +[nbar(:present-simple) .[intensifier(:present-simple) +extremely .true] +business]] +[vp(:present-simple) +tells .[nbar4(:present-simple) +cases .[comp1(:present-simple) +that .[s-slash(:present-simple) .[np(:present-simple) .the +[nbar(:present-simple) .[intensifier(:present-simple) +extremely .recent] +country]] +[vp-aux-slash(:present-simple) +would .work]]]]]]



user=> (load "babylon/nederlands")
nil
user=> (babylon.nederlands/demo)
Het kind.
[np .het +kind]

Een groet kind.
[np .een +[nbar .groet +kind]]

false
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
