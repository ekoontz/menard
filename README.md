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
nREPL server started on port 54167 on host 127.0.0.1 - nrepl://127.0.0.1:54167
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
De groep.
The group.

De jongen.
The boy.

De gebeiden.
The areas.

Een land.
A country.

De overheden.
The governments.

Het land.
The country.

Een been.
A leg.

De dag.
The day.

De kinderen.
The children.

De katten.
The cats.

Een lieve tafel.
A nice table.

De stomme feitten.
The stupid facts.

Het oude huis.
The old house.

De slimme hond.
The smart dog.

Een stom geld.
A stupid money.

Een lieve groep.
A nice group.

Een slimme jas.
A smart coat.

De kleine katten.
The small cats.

De vieze groepen.
The dirty groups.

Een kleine stoel.
A small chair.

De stomme stomme plaatsen.
The stupid stupid lots.

De kleine oude tafels.
The small old tables.

De oude stomme jassen.
The old stupid coats.

De stomme oude oogen.
The stupid old eyes.

De lieve stomme thuizen.
The nice stupid homes.

De oude oude heer.
The old old gentleman.

Een kleine vieze baby.
A small dirty baby.

Het vieze vieze bedrijf.
The dirty dirty business.

Een vieze stomme groep.
A dirty stupid group.

De stomme oude mannen.
The stupid old men.

Een heel oud huis.
A very old house.

Een heel klein dag.
A very small day.

Een heel vies meisje.
A very dirty girl.

Een heel lief druif.
A very nice grape.

Een heel slim gebeid.
A very smart area.

Een heel vies baan.
A very dirty job.

Een heel oud familie.
A very old family.

Een heel lief man.
The very nice man.

Een heel vies oog.
A very dirty eye.

Een heel klein boek.
A very small book.

Het slaapt.
It sleeps.

Jullie slaapen.
You sleep.

U werkt.
You work.

Het zingt.
It sings.

Saskia werkt.
Saskia works.

Ik werk.
I work.

Jullie zingen.
You sing.

U zingt.
You sing.

Saskia zingt.
Saskia sings.

Ik zing.
I sing.

De lieve hand werkt.
The nice hand works.

Een stomme maand slaapt.
A stupid month sleeps.

De kleine baby's zingen.
The small babies sing.

Het sterke land zingt.
The strong country sings.

De vieze overheid slaapt.
The dirty government sleeps.

De stomme zaken werken.
The stupid cases work.

De slimme plaats zingt.
The smart lot sings.

Het oude gebeid werkt.
The old area works.

De grote moeder zingt.
The big mother sings.

Een stomme hond werkt.
A stupid dog works.

Het kleine sterke geld slaapt.
The small strong money sleeps.

De sterke kleine stoel slaapt.
The strong small chair sleeps.

Een lieve grote moeder zingt.
A nice big mother sings.

De grote sterke honden werken.
The big strong dogs work.

Een oud klein leven slaapt.
An old small life sleeps.

De sterke kleine overheden zingen.
The strong small governments sing.

De sterke oude zaken slaapen.
The strong old cases sleep.

De oude sterke handen slaapen.
The old strong hands sleep.

De slimme vieze haren slaapen.
The smart dirty hairs sleep.

Een oud klein kind werkt.
An old small child works.

7
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
