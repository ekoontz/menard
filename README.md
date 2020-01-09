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
user=> (load "babylon")
nil
user=> (babylon/demo)
# 'een huis'; 1 example:
---
Een huis.|
         |A house.

# NP with no modifiers; 10 examples:
---
Onze mannen.|
            |Our men.
Uw boeken.|
          |Your books.
Hun jassen.|
           |Their coats.
Hun dame.|
         |Their lady.
Deze landen.|
            |These countries.
De mannen.|
          |The men.
De feitten.|
           |The facts.
Mijn gebeid.|
            |My area.
Deze tafel.|
           |This table.
Een vrouw.|
          |A woman.

# 'de grote boeken'; 1 example:
---
De grote boeken.|
                |The big books.

# NP with one modifier; 10 examples:
---
Deze lieve oogen.|
                 |These nice eyes.
Onze vieze familie.|
                   |Our dirty family.
Hun slimme levens.|
                  |Their smart lives.
Hun grote meisjes.|
                  |Their big girls.
Hun oude hond.|
              |Their old dog.
Uw stomme kittens.|
                  |Your stupid kittens.
De slimme overheden.|
                    |The smart governments.
Zijn enge geld.|
               |His scary money.
Mijn enge jas.|
              |My scary coat.
De oude zaken.|
              |The old cases.

# 'een heel klein druif'; 1 example:
---
Een heel klein druif.|
                     |A very small grape.

# NP with one modifier, which is itself modified; 10 examples:
---
Jouw echt slim dames.|
                     |Your really smart ladies.
Zijn heel eng zaken.|
                    |His very scary cases.
Zijn eigenlijk slim plaatsen.|
                             |His actually smart lots.
De erg vies kittens.|
                    |The very dirty kittens.
Zijn eigenlijk vies tafel.|
                          |His actually dirty table.
Mijn eigenlijk klein thuis.|
                           |My actually small home.
Uw ongewoon slim jongens.|
                         |Your unusually smart boys.
Hun eigenlijk vies huis.|
                        |Their actually dirty house.
De heel lief jassen.|
                    |The very nice coats.
Mijn ongewoon lief druiven.|
                           |My unusually nice grapes.

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
Onze eigenlijk waar sterke huizen slapen.|
                                         |Our actually true strong houses sleep.
Die heel waar kleine jas werkt.|
                               |That very true small coat works.
Uw ongewoon klein grote groepen werken.|
                                       |Your unusually small big groups work.
Zijn ongewoon waar oude landen slapen.|
                                      |His unusually true old countries sleep.
Uw echt sterk oude kat leest.|
                             |Your really strong old cat reads.
Mijn heel klein vieze leven slaapt.|
                                   |My very small dirty life sleeps.
Deze erg vies vieze jongen zingt.|
                                 |This very dirty dirty boy sings.
Deze erg lief sterke families slapen.|
                                     |These very nice strong families sleep.
Zijn ongewoon eng vieze haren zingen.|
                                     |His unusually scary dirty hairs sing.
Zijn heel sterk enge feitten slapen.|
                                    |His very strong scary facts sleep.

# Sentence whose subject has only one modifier.; 10 examples:
---
Een vieze hond slaapt.|
                      |A dirty dog sleeps.
Zijn stomme heren slapen.|
                         |Its stupid gentlemen sleep.
Haar kleine druif zingt.|
                        |Her small grape sings.
Hun vieze stoel werkt.|
                      |Their dirty chair works.
De enge heer zingt.|
                   |The scary man sings.
Uw waare jassen zien.|
                     |Your true coats see.
Jouw vieze mannen zien.|
                       |Your dirty men see.
Dat stomme thuis leest.|
                       |That stupid home reads.
Hun lieve benen werken.|
                       |Their nice legs work.
Mijn enge kind zingt.|
                     |My scary child sings.

# Sentence with non-reflexive direct object; 10 examples:
---
Jullie zien die.|
                |You see them.
Saskia ziet die.|
                |Saskia sees them.
Ik zie u.|
         |I see you.
Het ziet jou.|
             |It sees you.
Jullie zien hen.|
                |You see him.
U ziet hen.|
           |You see him.
U ziet jou.|
           |You see you.
U ziet die.|
           |You see them.
Saskia ziet u.|
              |Saskia sees you.
Het ziet Saskia.|
                |It sees Saskia.

# Sentence with reflexive direct object; 10 examples:
---
Wij zien ons.|
             |We see ourselves.
Jij ziet je.|
            |You see yourself.
Jullie zien je.|
               |You see yourselves.
Guus ziet zich.|
               |Guus sees himself.
Ik zie me.|
          |I see myself.
U ziet u.|
         |You see yourselves.
U ziet u.|
         |You see yourselves.
Jij ziet je.|
            |You see yourself.
Jullie zien je.|
               |You see yourselves.
Hij ziet zich.|
              |He sees himself.

# Sentence that looks like: [s [np een kat] [vp v ziet [np de muis]]]; 10 examples:
---
Die landen lezen die oogen.|
                           |Those countries read that eye.
Jouw land leest haar familie.|
                             |Your country reads her families.
Haar tafels zien hun gelden.|
                            |Her tables see their moneys.
Zijn jongen leest dat meisje.|
                             |His boy reads that girl.
Een gebeid leest zijn plaats.|
                             |An area reads his lot.
Een feit leest uw druiven.|
                          |A fact reads your grape.
Uw fiets ziet mijn groepen.|
                           |Your bike sees my group.
De banen lezen jouw jas.|
                        |The jobs read your coat.
Mijn bedrijf ziet onze moeder.|
                              |My business sees our mother.
Mijn fietsen zien uw kittens.|
                             |My bikes see your kittens.

13
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
