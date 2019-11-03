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

## Demo

For the demo, a sentence is generated for each specification listed in
<a href="https://github.com/ekoontz/babylon/blob/master/src/babylon/english/expressions.edn">expressions.edn</a>. 
Each expression is printed out first in its surface form, then in its the syntax tree form, as shown in the output below:

```
% lein repl
nREPL server started on port 61289 on host 127.0.0.1 - nrepl://127.0.0.1:61289
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
Who do nights teach?
[s-wh-interog .who +[s-interog-slash +do .[s-comp-2 .nights +teach]]]

false
user=> Who do rights look at?
[s-wh-interog .who +[s-interog-slash +do .[s-comp-2 .rights +[vp-slash +look .at]]]]

Words hope days would.
[s(:present-simple) .words +[vp(:present-simple) +hope .[s(:present-simple) .days +would]]]

At who do you look?
[s-wh-interog .[pp +at .who] +[s-interog-slash +do .[s-comp-2 .you +look]]]

Do businesses need to keep?
[s-interog +do .[s-comp .businesses +[vp-modal-1 +need .[infinitive +to .keep]]]]

Families say that businesses would.
[s(:present-simple) .families +[vp(:present-simple) +say .[comp2(:present-simple) +that .[s(:present-simple) .businesses +would]]]]

Mothers want to move sheep.
[s(:modal-present) .mothers +[vp-modal-1(:modal-present) +want .[infinitive(:modal-present) +to .[vp(:modal-present) +move .sheep]]]]

Lots move the women that heard.
[s(:present-simple) .lots +[vp(:present-simple) +move .[np(:present-simple) .the +[nbar3(:present-simple) +women .[comp1(:present-simple) +that .heard]]]]]

The whole bike would do a local month.
[s(:conditional) .[np(:conditional) .the +[nbar(:conditional) .whole +bike]] +[vp-aux(:conditional) +would .[vp(:conditional) +do .[np(:conditional) .a +[nbar(:conditional) .local +month]]]]]

The long gentleman takes states that the interesting puppies would show.
[s(:present-simple) .[np(:present-simple) .the +[nbar(:present-simple) .long +gentleman]] +[vp(:present-simple) +takes .[nbar4(:present-simple) +states .[comp1(:present-simple) +that .[s-slash(:present-simple) .[np(:present-simple) .the +[nbar(:present-simple) .interesting +puppies]] +[vp-aux-slash(:present-simple) +would .show]]]]]]

user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
