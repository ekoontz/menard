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
nREPL server started on port 65440 on host 127.0.0.1 - nrepl://127.0.0.1:65440
REPL-y 0.4.3, nREPL 0.6.0
Clojure 1.10.1
OpenJDK 64-Bit Server VM 12.0.1+12
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (do (load "babylon/translate") (babylon.translate/demo))
# intensifier adjective; 1 example:
---
Echt telurgesteld.|
                  |Really disappointed.

# det noun; 1 example:
---
Haar zaken.|
           |Her cases.

# noun verb; 1 example:
---
Saskia probeert.|
                |Saskia tries.

# det noun | verb; 1 example:
---
Haar meisje werkt.|
                  |Her girl works.

# det | adj noun; 1 example:
---
De sterke jassen.|
                 |The strong coats.

# 'een huis'; 1 example:
---
Een huis.|
         |A house.

# 'de grote boeken'; 1 example:
---
De grote boeken.|
                |The big books.

# 'een heel klein druif'; 1 example:
---
Een erg klein druif.|
                    |A very small grape.

# det [intensifier adj | adj noun]; 1 example:
---
Jouw behoorlijk slim sterke fietsen.|
                                    |Your quite smart strong bikes.

# 'De heel sterk slimme vrouen zingen'.; 1 example:
---
De erg sterk slimme vrouwen zingen.|
                                   |The very strong smart women sing.

# [det [intensifier adj | adj noun]] verb; 5 examples:
---
Die echt vies slimme bedrijven slapen.|
                                      |Those really dirty smart businesses sleep.
Die echt zelfverzekerd zelfverzekerde dag slaapt.|
                                                 |That really confident confident day sleeps.
Onze ongewoon nieuwsgierig ernstige ontwikkelingen slapen.|
                                                          |Our unusually curious serious developments sleep.
Hun echt verward zenuwachtige druif werkt.|
                                          |Their really confused nervous grape works.
Het echt waar bedroefde kind leest.|
                                   |The really true sad child reads.

# [det | adj noun] verb; 5 examples:
---
Die vieze meisjes veroorzaaken.|
                               |Those dirty girls cause.
Haar telurgestelde benen lezen.|
                               |Her disappointed legs read.
Dat stomme bedrijf zingt.|
                         |That stupid business sings.
Onze ernstige hand ziet.|
                        |Our serious hand sees.
Zijn ongeruste geld werkt.|
                          |His anxious money works.

# Sentence with object; 5 examples:
---
Guus ziet mij.|
              |Guus sees me.
Zij ziet hem.|
             |She sees him.
Ik zie u.|
         |I see you.
Hij ziet die.|
             |He sees them.
Jij ziet haar.|
              |You see her.

# Sentence with reflexive object; 5 examples:
---
Ik zie me.|
          |I see myself.
Ik zie me.|
          |I see myself.
U ziet u.|
         |You see yourselves.
Het ziet zich.|
              |It sees itself.
Jij ziet je.|
            |You see yourself.

# [s np [vp v np]]; 5 examples:
---
Jouw ontwikkeling leest die baan.|
                                 |Your development reads that job.
Haar handen zien uw thuis.|
                          |Her hands see your home.
Zijn dagen lezen de zaken.|
                          |His days read the cases.
Haar vrouwen veroorzaaken zijn benen.|
                                     |Her women cause his legs.
De benen lezen onze plaatsen.|
                             |The legs read our lots.

# [s n [vp v [vp:inf to v]]]; 5 examples:
---
Zij probeert te zingen.|
                       |She tries to sing.
Zij probeert te komen.|
                      |She tries to come.
Jij probeert te lezen.|
                      |You try to read.
Hij probeert te lezen.|
                      |He tries to read.
Ik probeer te veroorzaaken.|
                           |I try to cause.

# e.g. Jouw families probeeren mijn honden te zien.; 5 examples:
---
Die families probeeren uw baby's te zien.|
                                         |Those families try to see your babies.
Zijn handen probeeren hun thuis te zien.|
                                        |His hands try to see their home.
Dit huis probeert een jas te veroorzaaken.|
                                          |This house tries to cause a coat.
Die baan probeert hun dagen te lezen.|
                                     |That job tries to read their days.
Hun handen probeeren die lezen te probeeren.|
                                            |Their hands try to try to read them.

17
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
