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

For the demo, a Dutch sentence is generated for each specification listed in
<a href="https://github.com/ekoontz/babylon/blob/master/src/babylon/nederlands/expressions.edn">expressions.edn</a>. 
Each expression is then translated into English, as shown in the output below:

```
% lein repl
nREPL server started on port 63899 on host 127.0.0.1 - nrepl://127.0.0.1:63899
REPL-y 0.4.3, nREPL 0.6.0
Clojure 1.10.1
OpenJDK 64-Bit Server VM 12.0.1+12
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (load "babylon")
nil
user=> (babylon/demo)
;; 'een huis'
Een huis.
A house.
nil
;; generalization of the above:
Het feit.
The fact.
nil
;; 'het grote boeken'
De grote boeken.
The big books.
nil
;; generalization of the above:
De grote baby.
The big baby.
nil
;; 'een heel klein druif'
Een heel klein druif.
A very small grape.
nil
;; generalization of the above:
De heel stom thuizen.
The very stupid homes.
nil
;; 'De heel klein oud fietsen'.
De heel klein oude fietsen.
The very small old bikes.
nil
;; generalization of the above:
De heel slim lieve tafels.
The very smart nice tables.
nil
;; 'De heel sterk slimme vrouen zingen'.
De heel sterk slimme vrouwen zingen.
Some very strong smart women sing.
nil
;; generalization of the above:
De heel groot kleine boeken slaapen.
The very big small books sleep.
nil
10
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
