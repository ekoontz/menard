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
user=> (babylon.translate/demo)
# 'een huis'; 1 example:
---
Een huis.|
         |A house.

# NP with no modifiers; 10 examples:
---
De stoelen.|
           |The chairs.
Die groepen.|
            |Those groups.
Die moeders.|
            |Those mothers.
Zijn fietsen.|
             |His bikes.
Zijn gebeiden.|
              |His areas.
De dame.|
        |The lady.
Uw kinderen.|
            |Your children.
Haar huis.|
          |Her house.
Hun honden.|
           |Their dogs.
Dit been.|
         |This leg.

# 'de grote boeken'; 1 example:
---
De grote boeken.|
                |The big books.

# NP with one modifier; 10 examples:
---
Die lieve hond.|
               |That nice dog.
Dat grote bedrijf.|
                  |That big business.
Mijn kleine zaken.|
                  |My small cases.
Haar vieze leven.|
                 |Her dirty life.
Uw lieve plaats.|
                |Your nice lot.
Uw kleine families.|
                   |Your small families.
Mijn lieve hond.|
                |My nice dog.
Zijn vieze haren.|
                 |His dirty hairs.
Zijn lieve plaats.|
                  |His nice lot.
Zijn lieve oog.|
               |Its nice eye.

# 'een heel klein druif'; 1 example:
---
Een erg klein druif.|
                    |A very small grape.

# NP with one modifier, which is itself modified; 10 examples:
---
Het erg lief bedrijf.|
                     |The very nice business.
Jouw echt sterk katten.|
                       |Your really strong cats.
Zijn erg oud bedrijf.|
                     |Its very old business.
Deze erg klein banen.|
                     |These very small jobs.
Deze ongewoon klein plaatsen.|
                             |These unusually small lots.
Onze erg groot meisjes.|
                       |Our very big girls.
Deze heel lief hand.|
                    |This very nice hand.
Zijn echt sterk meisje.|
                       |Its really strong girl.
Zijn heel klein thuizen.|
                        |Its very small homes.
Deze heel oud dame.|
                   |This very old lady.

# 'De heel klein oud fietsen'.; 1 example:
---
De erg klein oude fietsen.|
                          |The very small old bikes.

# 'De heel sterk slimme vrouen zingen'.; 1 example:
---
De erg sterk slimme vrouwen zingen.|
                                   |The very strong smart women sing.

# Sentence whose subject has two modifiers, the first of which is itself modified; 10 examples:
---
Uw heel lief stomme bedrijf ziet.|
                                 |Your very nice stupid business sees.
Die eigenlijk slim lieve mannen slapen.|
                                       |Those actually smart nice men sleep.
Die heel groot vieze gebeiden werken.|
                                     |Those very big dirty areas work.
Uw erg slim grote dag werkt.|
                            |Your very smart big day works.
Uw eigenlijk klein oude leven ziet.|
                                   |Your actually small old life sees.
Mijn heel groot grote jongens zien.|
                                   |My very big big boys see.
De eigenlijk slim oude baby's slapen.|
                                     |The actually smart old babies sleep.
Haar eigenlijk oud grote kitten werkt.|
                                      |Her actually old big kitten works.
Jouw erg vies stomme kat leest.|
                               |Your very dirty stupid cat reads.
Dit eigenlijk oud sterke leven leest.|
                                     |This actually old strong life reads.

# Sentence with non-reflexive direct object; 10 examples:
---
U ziet hun.|
           |You see him.
Jij ziet mij.|
             |You see me.
Wij zien haar.|
              |We see her.
U ziet hen.|
           |You see him.
Jullie zien Guus.|
                 |You see Guus.
Ik zie die.|
           |I see them.
Het ziet mij.|
             |It sees me.
U ziet jou.|
           |You see you.
Zij ziet u.|
           |She sees you.
Jij ziet hun.|
             |You see him.

# Sentence with reflexive direct object; 10 examples:
---
Ik zie me.|
          |I see myself.
Ik zie me.|
          |I see myself.
Het ziet zich.|
              |It sees itself.
Wij zien ons.|
             |We see ourselves.
Wij zien ons.|
             |We see ourselves.
Het ziet zich.|
              |It sees itself.
Zij ziet zich.|
              |She sees herself.
Ik zie me.|
          |I see myself.
Zij zien zich.|
              |They see themselves.
Zij ziet zich.|
              |She sees herself.

11
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
