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
nREPL server started on port 60458 on host 127.0.0.1 - nrepl://127.0.0.1:60458
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
What does who seem?
[s-wh-interog .what +[s-interog-slash +does .[s-comp-2 .who +seem]]]

Governments would not put.
[s(:conditional) .governments +[vp-aux(:conditional) +would .[negp(:conditional) +not .put]]]

At what did points look?
[s-wh-interog .[pp +at .what] +[s-interog-slash +did .[s-comp-2 .points +look]]]

Moneys believe parts would.
A very better fact begins him.
[s(:present-simple) .moneys +[vp(:present-simple) +believe .[s(:present-simple) .parts +would]]]
[s(:present-simple) .[np(:present-simple) .a +[nbar(:present-simple) .[intensifier(:present-simple) +very .better] +fact]] +[vp(:present-simple) +begins .him]]


What did studies talk to?
[s-wh-interog .what +[s-interog-slash +did .[s-comp-2 .studies +[vp-slash +talk .to]]]]

Did bikes try to hear?
Programs think that programs would.
[s(:present-simple) .programs +[vp(:present-simple) +think .[comp2(:present-simple) +that .[s(:present-simple) .programs +would]]]]

[s-interog +did .[s-comp .bikes +[vp-modal-1 +try .[infinitive +to .hear]]]]

Things were not teaching.
States tell the puppies that meant.
[s(:past-progressive) .things +[vp-aux(:past-progressive) +were .[negp(:past-progressive) +not .teaching]]]

[s(:present-simple) .states +[vp(:present-simple) +tell .[np(:present-simple) .the +[nbar3(:present-simple) +puppies .[comp1(:present-simple) +that .meant]]]]]

I was not knowing myself.
[s(:past-progressive) .I +[vp-aux(:past-progressive) +was .[negp(:past-progressive) +not .[vp(:past-progressive) +knowing .myself]]]]

Gentlemen want to hope men would.
[s(:modal-present) .gentlemen +[vp-modal-1(:modal-present) +want .[infinitive(:modal-present) +to .[vp(:modal-present) +hope .[s(:modal-present) .men +would]]]]]

The extremely real gentleman would play an able boy.
[s(:conditional) .[np(:conditional) .the +[nbar(:conditional) .[intensifier(:conditional) +extremely .real] +gentleman]] +[vp-aux(:conditional) +would .[vp(:conditional) +play .[np(:conditional) .a +[nbar(:conditional) .able +boy]]]]]

A very different fact sit on a boy.
[s(:modal) .[np(:modal) .a +[nbar(:modal) .[intensifier(:modal) +very .different] +fact]] +[vp-adjunct(:modal) +sit .[pp(:modal) +on .[np(:modal) .a +boy]]]]

The very certain words feel chairs that a very sure fact would know.
[s(:present-simple) .[np(:present-simple) .the +[nbar(:present-simple) .[intensifier(:present-simple) +very .certain] +words]] +[vp(:present-simple) +feel .[nbar4(:present-simple) +chairs .[comp1(:present-simple) +that .[s-slash(:present-simple) .[np(:present-simple) .a +[nbar(:present-simple) .[intensifier(:present-simple) +very .sure] +fact]] +[vp-aux-slash(:present-simple) +would .know]]]]]]

15
user=> (load "babylon/nederlands")
nil
user=> (babylon.nederlands/demo)
De kinderen.
[np .de +kinderen]

De vieze haar.
[np .de +[nbar .vieze +haar]]

2
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
