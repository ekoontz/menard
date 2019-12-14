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
nREPL server started on port 54579 on host 127.0.0.1 - nrepl://127.0.0.1:54579
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
De heren.|
         |The gentlemen.
De jassen.|
          |The coats.
Een fiets.|
          |A bike.
De plaatsen.|
            |The lots.
De groepen.|
           |The groups.
De jongen.|
          |The boy.
Een bedrijf.|
            |A business.
Een fiets.|
          |A bike.
De vrouwen.|
           |Some women.
De zaken.|
         |The cases.

# 'de grote boeken'; 1 example:
---
De grote boeken.|
                |The big books.

# NP with one modifier; 10 examples:
---
De sterke moeders.|
                  |The strong mothers.
De vieze moeders.|
                 |The dirty mothers.
Het kleine oog.|
               |The small eye.
De lieve dagen.|
               |The nice days.
Een lieve kitten.|
                 |A nice kitten.
Een oude hond.|
              |An old dog.
De oude thuizen.|
                |The old homes.
De vieze meisjes.|
                 |The dirty girls.
De stomme dagen.|
                |The stupid days.
De lieve haren.|
               |The nice hairs.

# 'een heel klein druif'; 1 example:
---
Een heel klein druif.|
                     |A very small grape.

# NP with one modifier, which is itself modified; 10 examples:
---
De heel groot boeken.|
                     |The very big books.
De eigenlijk vies vrouwen.|
                          |The actually dirty women.
De heel slim overheid.|
                      |The very smart government.
Het eigenlijk klein huis.|
                         |The actually small house.
De ongewoon lief groepen.|
                         |The unusually nice groups.
De ongewoon stom haren.|
                       |The unusually stupid hairs.
De ongewoon slim haren.|
                       |The unusually smart hairs.
De eigenlijk klein hond.|
                        |The actually small dog.
De echt stom stoelen.|
                     |The really stupid chairs.
De eigenlijk lief druif.|
                        |The actually nice grape.

# 'De heel klein oud fietsen'.; 1 example:
---
De heel klein oude fietsen.|
                           |The very small old bikes.

# 'De heel sterk slimme vrouen zingen'.; 1 example:
---
De heel sterk slimme vrouwen zingen.|
                                    |Some very strong smart women sing.

# Sentence whose subject has two modifiers, the first of which is itself modified; 10 examples:
---
De heel klein vieze kinderen zien.|
                                  |The very small dirty children see.
De echt sterk stomme kittens zingen.|
                                    |The really strong stupid kittens sing.
De ongewoon lief sterke jassen slaapen.|
                                       |The unusually nice strong coats sleep.
De eigenlijk sterk grote maanden werken.|
                                        |The actually strong big months work.
De echt stom grote baby slaapt.|
                               |The really stupid big baby sleeps.
De echt lief vieze landen slaapen.|
                                  |The really nice dirty countries sleep.
De echt groot kleine haren slaapen.|
                                   |The really big small hairs sleep.
De eigenlijk slim stomme katten slaapen.|
                                        |The actually smart stupid cats sleep.
De echt sterk slimme oogen werken.|
                                  |The really strong smart eyes work.
Een eigenlijk sterk kleine kat ziet.|
                                    |An actually strong small cat sees.

# Sentence with non-reflexive direct object; 10 examples:
---
Het ziet Guus.|
              |It sees Guus.
Wij zien die.|
             |We see them.
Guus ziet die.|
              |Guus sees them.
Het ziet hun.|
             |It sees him.
Jij ziet hem.|
             |You see him.

# Sentence with reflexive direct object; 10 examples:
---
Zij ziet zich.|
              |She sees herself.
Ik zie me.|
          |I see myself.
Ik zie me.|
          |I see myself.
U ziet u.|
         |You see yourselves.
U ziet u.|
         |You see yourselves.
U ziet u.|
         |You see yourselves.
Hij ziet zich.|
              |He sees himself.
U ziet u.|
         |You see yourself.
Wij zien ons.|
             |We see ourselves.
Hij ziet zich.|
              |He sees himself.

11
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
