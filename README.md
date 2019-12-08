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
Een been.|
         |A leg.
De bedrijven.|
             |The businesses.
Een baby.|
         |A baby.
De benen.|
         |The legs.
Een kitten.|
           |A kitten.
De grote boeken.|
                |The big books.
De slimme gebeiden.|
                   |The smart areas.
De stomme huizen.|
                 |The stupid houses.
De vieze baan.|
              |The dirty job.
De oude zaak.|
             |The old case.
De oude benen.|
              |The old legs.
Een heel klein druif.|
                     |A very small grape.
De heel stom landen.|
                    |The very stupid countries.
De heel sterk jongens.|
                      |The very strong boys.
De heel oud familie.|
                    |The very old family.
Een heel klein groep.|
                     |A very small group.
De heel klein tafels.|
                     |The very small tables.
De heel klein oude fietsen.|
                           |The very small old bikes.
De heel klein vieze heren.|
                          |Some very small dirty men.
De heel vies vieze overheden.|
                             |The very dirty dirty governments.
Een heel lief oude overheid.|
                            |A very nice old government.
Een heel vies slimme stoel.|
                           |A very dirty smart chair.
De heel oud vieze jassen.|
                         |The very old dirty coats.
De heel sterk slimme vrouwen zingen.|
                                    |The very strong smart women sing.
Een heel sterk kleine stoel slaapt.|
                                   |A very strong small chair sleeps.
De heel slim grote heren slaapen.|
                                 |Some very smart big gentlemen sleep.
De heel klein lieve stoelen werken.|
                                   |The very small nice chairs work.
De heel sterk grote huizen zingen.|
                                  |The very strong big houses sing.
Een heel groot vieze dame zient.|
                                |A very big dirty lady sees.
Jullie zienen hen.|
                  |You see him.
Jullie zienen u.|
                |You see you.
Wij zienen Guus.|
                |We see Guus.
Hij zient haar.|
               |He sees her.
Wij zienen Saskia.|
                  |We see Saskia.
11
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
