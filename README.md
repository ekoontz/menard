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
nREPL server started on port 59314 on host 127.0.0.1 - nrepl://127.0.0.1:59314
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
De huizen.
The houses.

De benen.
The legs.

Een jas.
A coat.

De dagen.
The days.

De overheid.
The government.

De thuizen.
The homes.

De benen.
The legs.

Een familie.
A family.

Een overheid.
A government.

De feitten.
The facts.

De vieze jongens.
The dirty boys.

De lieve dag.
The nice day.

Een oude baan.
An old job.

De vieze tafel.
The dirty table.

De sterke haren.
The strong hairs.

De stomme katten.
The stupid cats.

De oude bedrijven.
The old businesses.

Het oude bedrijf.
The old business.

Een grote jongen.
A big boy.

De oude benen.
The old legs.

Een grote oude moeder.
A big old mother.

De kleine oude jongen.
The small old boy.

De slimme kleine honden.
The smart small dogs.

Een stomme stomme kat.
A stupid stupid cat.

De grote slimme dame.
The big smart lady.

Een lieve grote kitten.
A nice big kitten.

De lieve vieze levens.
The nice dirty lives.

De kleine oude families.
The small old families.

Een vies stom land.
A dirty stupid country.

De stomme grote druif.
The stupid big grape.

Zij werkt.
She works.

Hij zingt.
He sings.

Jij zingt.
You sing.

Zij slaapen.
They sleep.

U slaapt.
You sleep.

Zij werkt.
She works.

Jij werkt.
You work.

Jullie slaapen.
You sleep.

Zij zingt.
She sings.

Hij werkt.
He works.

Het lieve geld zingt.
The nice money sings.

Een grote zaak werkt.
A big case works.

Een oud kind slaapt.
An old child sleeps.

De oude gebeiden zingen.
The old areas sing.

Het oude gebeid zingt.
The old area sings.

De vieze heer slaapt.
The dirty man sleeps.

Een grote tafel werkt.
A big table works.

De vieze oogen slaapen.
The dirty eyes sleep.

De grote thuizen werken.
The big homes work.

De vieze dagen werken.
The dirty days work.

De sterke oude zaken slaapen.
The strong old cases sleep.

Een slimme sterke baan werkt.
A smart strong job works.

Een lieve stomme groep zingt.
A nice stupid group sings.

Een lieve slimme jas zingt.
A nice smart coat sings.

Een sterke grote maand slaapt.
A strong big month sleeps.

De stomme sterke banen slaapen.
The stupid strong jobs sleep.

De sterke stomme gelden zingen.
The strong stupid moneys sing.

De sterke slimme zaak slaapt.
The strong smart case sleeps.

Een kleine lieve baby slaapt.
A small nice baby sleeps.

De oude vieze banen slaapen.
The old dirty jobs sleep.

6
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
