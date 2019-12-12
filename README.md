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
De jongens.|
           |The boys.
Een baan.|
         |A job.
De honden.|
          |The dogs.
De banen.|
         |The jobs.
Een tafel.|
          |A table.
De man.|
       |The man.
Een zaak.|
         |A case.
De hand.|
        |The hand.
De tafel.|
         |The table.
Het leven.|
          |The life.

# 'de grote boeken'; 1 example:
---
De grote boeken.|
                |The big books.

# NP with one modifier; 10 examples:
---
Een kleine tafel.|
                 |A small table.
De sterke plaatsen.|
                   |The strong lots.
De lieve zaak.|
              |The nice case.
De kleine druif.|
                |The small grape.
Een slimme plaats.|
                  |A smart lot.
Een stom kind.|
              |The stupid child.
Een slimme zaak.|
                |A smart case.
De vieze heren.|
               |Some dirty gentlemen.
Een stomme man.|
               |The stupid man.
De lieve groepen.|
                 |The nice groups.

# 'een heel klein druif'; 1 example:
---
Een heel klein druif.|
                     |A very small grape.

# NP with one modifier, which is itself modified; 10 examples:
---
Een heel oud maand.|
                   |A very old month.
De heel sterk kat.|
                  |The very strong cat.
De heel stom druiven.|
                     |The very stupid grapes.
De heel vies maanden.|
                     |The very dirty months.
De heel sterk dame.|
                   |The very strong lady.
De heel slim boeken.|
                    |The very smart books.
De heel vies hand.|
                  |The very dirty hand.
De heel klein landen.|
                     |The very small countries.
Het heel klein meisje.|
                      |The very small girl.
De heel klein maanden.|
                      |The very small months.

# 'De heel klein oud fietsen'.; 1 example:
---
De heel klein oude fietsen.|
                           |The very small old bikes.

# NP with two modifiers, the first of which is itself modified; 10 examples:
---
De heel vies oude dame.|
                       |The very dirty old lady.
De heel vies stomme katten.|
                           |The very dirty stupid cats.
Het heel sterk grote oog.|
                         |The very strong big eye.
De heel oud kleine boeken.|
                          |The very old small books.
Een heel klein kleine fiets.|
                            |A very small small bike.
Het heel klein kleine oog.|
                          |The very small small eye.
De heel vies sterke mannen.|
                           |The very dirty strong men.
De heel oud kleine honden.|
                          |The very old small dogs.
Een heel sterk slimme kat.|
                          |A very strong smart cat.
De heel sterk sterke feitten.|
                             |The very strong strong facts.

# 'De heel sterk slimme vrouen zingen'.; 1 example:
---
De heel sterk slimme vrouwen zingen.|
                                    |The very strong smart women sing.

# Sentence whose subject has two modifiers, the first of which is itself modified; 10 examples:
---
De heel groot lieve plaats werkt.|
                                 |The very big nice lot works.
Een heel sterk oude kat zingt.|
                              |A very strong old cat sings.
De heel sterk sterke boeken werken.|
                                   |The very strong strong books work.
De heel oud vieze boeken zingen.|
                                |The very old dirty books sing.
De heel oud kleine plaatsen zien.|
                                 |The very old small lots see.
De heel oud sterke bedrijven werken.|
                                    |The very old strong businesses work.
Een heel vies vies land werkt.|
                              |A very dirty dirty country works.
Een heel lief stomme moeder zingt.|
                                  |A very nice stupid mother sings.
De heel stom slimme jongen zingt.|
                                 |The very stupid smart boy sings.
Een heel klein klein land zingt.|
                                |A very small small country sings.

# Sentence with direct object; 10 examples:
---
Ik zie me.|
          |I see myself.
Guus ziet zich.|
               |Guus sees himself.
Ik zie me.|
          |I see myself.
U ziet die.|
           |You see them.
Wij zien ons.|
             |We see ourselves.
Wij zien mij.|
             |We see me.
Jullie zien haar.|
                 |You see her.
Het ziet zich.|
              |It sees itself.
U ziet Saskia.|
              |You see Saskia.

# Sentence with reflexive direct object; 10 examples:
---
U ziet u.|
         |You see yourself.
Ik zie me.|
          |I see myself.
Ik zie me.|
          |I see myself.
U ziet u.|
         |You see yourselves.
Wij zien ons.|
             |We see ourselves.
U ziet u.|
         |You see yourself.
Wij zien ons.|
             |We see ourselves.
Zij ziet zich.|
              |She sees herself.
Het ziet zich.|
              |It sees itself.
Guus ziet zich.|
               |Guus sees himself.

12
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
