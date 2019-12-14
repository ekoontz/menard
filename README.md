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
nREPL server started on port 62620 on host 127.0.0.1 - nrepl://127.0.0.1:62620
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
# 'een huis'; 1 example:
---
Een huis.|
         |A house.

# NP with no modifiers; 10 examples:
---
Dat meisje.|
           |That girl.
De jassen.|
          |The coats.
Een boek.|
         |A book.
De zaken.|
         |The cases.
Die fietsen.|
            |Those bikes.
De dames.|
         |The ladies.
Een feit.|
         |A fact.
Deze plaats.|
            |This lot.
Die plaats.|
           |That lot.
Deze maand.|
           |This month.

# 'de grote boeken'; 1 example:
---
De grote boeken.|
                |The big books.

# NP with one modifier; 10 examples:
---
De lieve kittens.|
                 |The nice kittens.
Het kleine meisje.|
                  |The small girl.
Deze grote handen.|
                  |These big hands.
Deze grote kittens.|
                   |These big kittens.
Deze kleine mannen.|
                   |These small men.
De stomme fiets.|
                |The stupid bike.
Een lief kind.|
              |A nice child.
De vieze gebeiden.|
                  |The dirty areas.
Deze grote gelden.|
                  |These big moneys.
Een kleine man.|
               |A small man.

# 'een heel klein druif'; 1 example:
---
Een erg klein druif.|
                    |A very small grape.

# NP with one modifier, which is itself modified; 10 examples:
---
De heel lief tafels.|
                    |The very nice tables.
Dit erg vies bedrijf.|
                     |This very dirty business.
Die echt oud heren.|
                   |Those really old men.
Dat ongewoon slim haar.|
                       |That unusually smart hair.
De echt vies baan.|
                  |The really dirty job.
Dit eigenlijk vies feit.|
                        |This actually dirty fact.
Deze ongewoon stom vrouwen.|
                           |These unusually stupid women.
De echt vies banen.|
                   |The really dirty jobs.
Deze erg vies kinderen.|
                       |These very dirty children.
Een echt vies dame.|
                   |A really dirty lady.

# 'De heel klein oud fietsen'.; 1 example:
---
De erg klein oude fietsen.|
                          |The very small old bikes.

# 'De heel sterk slimme vrouen zingen'.; 1 example:
---
Die erg sterk slimme vrouwen zingen.|
                                    |Those very strong smart women sing.

# Sentence whose subject has two modifiers, the first of which is itself modified; 10 examples:
---
De erg oud sterke baby's slaapen.|
                                 |The very old strong babies sleep.
De erg slim kleine families zingen.|
                                   |The very smart small families sing.
De erg slim oude jas slaapt.|
                            |The very smart old coat sleeps.
Deze heel lief grote levens zingen.|
                                   |These very nice big lives sing.
Die eigenlijk stom lieve druiven werken.|
                                        |Those actually stupid nice grapes work.
Deze echt oud slimme heren zingen.|
                                  |These really old smart men sing.
De ongewoon sterk kleine plaats slaapt.|
                                       |The unusually strong small lot sleeps.
Deze echt klein stomme fiets zingt.|
                                   |This really small stupid bike sings.
Deze heel klein lieve familie werkt.|
                                    |This very small nice family works.
Deze erg groot sterke stoel werkt.|
                                  |This very big strong chair works.

# Sentence with non-reflexive direct object; 10 examples:
---
Jij ziet haar.|
              |You see her.
Wij zien hun.|
             |We see him.
Jullie zien die.|
                |You see them.
Jij ziet hem.|
             |You see him.
Jij ziet Guus.|
              |You see Guus.
Wij zien jou.|
             |We see you.
Wij zien hen.|
             |We see him.
Jullie zien Saskia.|
                   |You see Saskia.
Jij ziet Guus.|
              |You see Guus.
Het ziet die.|
             |_.

# Sentence with reflexive direct object; 10 examples:
---
Wij zien ons.|
             |We see ourselves.
Ik zie me.|
          |I see myself.
U ziet u.|
         |You see yourselves.
Hij ziet zich.|
              |He sees himself.
Wij zien ons.|
             |We see ourselves.
Wij zien ons.|
             |We see ourselves.
Ik zie me.|
          |I see myself.
Saskia ziet zich.|
                 |Saskia sees herself.
Ik zie me.|
          |I see myself.
Wij zien ons.|
             |We see ourselves.

11
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
