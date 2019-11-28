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
nREPL server started on port 58587 on host 127.0.0.1 - nrepl://127.0.0.1:58587
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
De gebeiden.
The areas.

De haren.
The hairs.

Een bedrijf.
A business.

Een leven.
A life.

De zaak.
The cases.

De fiets.
The bikes.

De jongen.
The boy.

Het leven.
The life.

Een kat.
A cat.

De kinderen.
The children.

Een kleine groep.
A small group.

Een kleine zaak.
A small case.

Een sterke jongen.
A strong boy.

Een vieze moeder.
A dirty mother.

De kleine familie.
The small family.

Het lieve meisje.
The nice girl.

De slimme maanden.
The smart months.

De slimme zaken.
The smart cases.

Het slimme land.
The smart country.

De kleine tafels.
The small tables.

De slimme slimme dame.
The smart smart lady.

De grote oude honden.
The big old dogs.

De slimme stomme overheid.
The smart stupid governments.

Een grote lieve overheid.
A big nice government.

Een lief sterk meisje.
A nice strong girl.

Het oude oude bedrijf.
The old old business.

De kleine slimme benen.
The small smart legs.

Een oude kleine maand.
An old small month.

Het grote kleine boek.
The big small book.

De grote lieve oogen.
The big nice eyes.

Saskia werkt.
Saskia works.

Hij werkt.
He works.

Saskia slaapt.
Saskia sleeps.

Zij slaapt.
She sleeps.

U werkt.
You work.

Zij werkt.
She works.

Jij werkt.
You work.

Wij zingen.
We sing.

Hij slaapt.
He sleeps.

Jij zingt.
You sing.

Het vieze feit zingt.
The dirty fact sings.

Een stomme druif zingt.
A stupid grape sings.

De grote plaatsen zingen.
The big lots sing.

De vieze huizen slaapen.
The dirty houses sleep.

Een grote familie slaapt.
A big family sleeps.

Een sterke kitten slaapt.
A strong kitten sleeps.

De vieze mannen slaapen.
The dirty men sleep.

Een kleine maand zingt.
A small month sings.

De grote boeken slaapen.
The big books sleep.

Een sterk bedrijf zingt.
A strong business sings.

De oude kleine overheden zingen.
The old small governments sing.

De vieze oude baby's werken.
The dirty old babies work.

De vieze stomme oogen zingen.
The dirty stupid eyes sing.

De slimme sterke dames werken.
The smart strong ladies work.

De kleine vieze haren zingen.
The small dirty hairs sing.

De vieze stomme levens slaapen.
The dirty stupid lives sleep.

De oude stomme tafels werken.
The old stupid tables work.

Een kleine stomme plaats werkt.
A small stupid lot works.

De vieze sterke tafels zingen.
The dirty strong tables sing.

Het kleine grote feit werkt.
The small big fact works.

6
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
