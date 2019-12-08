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

### HPSG

Based on a linguistic theory called HPSG (Head Driven Phrase Structure Grammar). More about HPSG:

- http://hpsg.stanford.edu/
- https://en.wikipedia.org/wiki/Head-driven_phrase_structure_grammar

### [Fehringer 1999]

Dutch grammar and lexicon based on [Carole Fehringer, "A Reference Grammar of Dutch", Cambridge University Press, 1999](https://books.google.nl/books/about/A_Reference_Grammar_of_Dutch.html?id=hXZNkFqILp0C&redir_esc=y). 

### Verbix

Uses verb conjugations from Verbix: http://www.verbix.com 

## Demo

For the demo, a Dutch sentence is generated for each specification listed in
<a href="https://github.com/ekoontz/babylon/blob/master/src/babylon/nederlands/expressions.edn">expressions.edn</a>. 
Each expression is then translated into English, as shown in the output below:

```
% lein repl
nREPL server started on port 63379 on host 127.0.0.1 - nrepl://127.0.0.1:63379
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
Een huis.|
         |A house.
Het thuis.|
          |The home.
De haren.|
         |The hairs.
Een dame.|
         |A lady.
De boeken.|
          |The books.
Een plaats.|
           |A lot.
De grote boeken.|
                |The big books.
Een oud meisje.|
               |An old girl.
Een lieve jongen.|
                 |A nice boy.
Een lieve heer.|
               |A nice man.
De kleine levens.|
                 |The small lives.
De grote banen.|
               |The big jobs.
Een heel klein druif.|
                     |A very small grape.
De heel klein dag.|
                  |The very small day.
Het heel groot huis.|
                    |The very big house.
De heel groot dames.|
                    |The very big ladies.
De heel groot plaatsen.|
                       |The very big lots.
De heel lief plaatsen.|
                      |The very nice lots.
De heel klein oude fietsen.|
                           |The very small old bikes.
Een heel oud slim geld.|
                       |A very old smart money.
De heel slim vieze gebeiden.|
                            |The very smart dirty areas.
Een heel stom vieze jongen.|
                           |A very stupid dirty boy.
Een heel stom grote stoel.|
                          |A very stupid big chair.
Het heel lief kleine thuis.|
                           |The very nice small home.
De heel sterk slimme vrouwen zingen.|
                                    |The very strong smart women sing.
Een heel vies stomme zaak ziet.|
                               |A very dirty stupid case sees.
De heel groot lieve katten zien.|
                                |The very big nice cats see.
De heel vies sterke overheden slaapen.|
                                      |The very dirty strong governments sleep.
De heel stom slimme hond werkt.|
                               |The very stupid smart dog works.
De heel vies stomme katten slaapen.|
                                   |The very dirty stupid cats sleep.
Jij ziet Saskia.|
                |You see Saskia.
U ziet haar.|
            |You see her.
U ziet hun.|
           |You see him.
Guus ziet hen.|
              |Guus sees him.
Saskia ziet u.|
              |Saskia sees you.
11
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
