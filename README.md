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
Each expression is printed out first in its surface form, then in its the syntax tree form.

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
What did boys have?
[s-wh-interog .what +[s-interog-slash +did .[s-comp-2 .boys +have]]]
true
user=> Men hope tables would.
[s(:present-simple) .men +[vp(:present-simple) +hope .[s(:present-simple) .tables +would]]]
At who did people look?
[s-wh-interog .[pp +at .who] +[s-interog-slash +did .[s-comp-2 .people +look]]]Who did stories look at?

[s-wh-interog .who +[s-interog-slash +did .[s-comp-2 .stories +[vp-slash +look .at]]]]
Numbers believe that governments would.
[s(:present-simple) .numbers +[vp(:present-simple) +believe .[comp2(:present-simple) +that .[s(:present-simple) .governments +would]]]]
It wants to begin houses.
[s(:modal-present) .it +[vp-modal-1(:modal-present) +wants .[infinitive(:modal-present) +to .[vp(:modal-present) +begin .houses]]]]
I have the study that became.
[s(:present-simple) .I +[vp(:present-simple) +have .[np(:present-simple) .the +[nbar3(:present-simple) +study .[comp1(:present-simple) +that .became]]]]]
The military worlds would walk the free area.
[s(:conditional) .[np(:conditional) .the +[nbar(:conditional) .military +worlds]] +[vp-aux(:conditional) +would .[vp(:conditional) +walk .[np(:conditional) .the +[nbar(:conditional) .free +area]]]]]
The black right hears rooms that the big problems would show.
[s(:present-simple) .[np(:present-simple) .the +[nbar(:present-simple) .black +right]] +[vp(:present-simple) +hears .[nbar4(:present-simple) +rooms .[comp1(:present-simple) +that .[s-slash(:present-simple) .[np(:present-simple) .the +[nbar(:present-simple) .big +problems]] +[vp-aux-slash(:present-simple) +would .show]]]]]]

user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
