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
nREPL server started on port 61060 on host 127.0.0.1 - nrepl://127.0.0.1:61060
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
Eigenlijk oud.|
              |Actually old.

# det noun; 1 example:
---
Die maanden.|
            |Those months.

# noun verb; 1 example:
---
Ik probeer.|
           |I try.

# det noun | verb; 1 example:
---
Jouw overheden werken.|
                      |Your governments work.

# det | adj noun; 1 example:
---
Haar grote zaken.|
                 |Her big cases.

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
Een heel klein druif.|
                     |A very small grape.

# det [intensifier adj | adj noun]; 1 example:
---
Haar heel waar slimme banen.|
                            |Her very true smart jobs.

# 'De heel sterk slimme vrouen zingen'.; 1 example:
---
De heel sterk slimme vrouwen zingen.|
                                    |The very strong smart women sing.

# [det [intensifier adj | adj noun]] verb; 5 examples:
---
De eigenlijk groot verlegene maanden zien.|
                                          |The actually big shy months see.
Die heel opgevonden vieze thuizen werken.|
                                         |Those very excited dirty homes work.
Zijn erg zelfverzekerd verwarde jongens probeeren.|
                                                  |His very confident confused boys try.
Onze eigenlijk klein telurgestelde beschavingen zingen.|
                                                       |Our actually small disappointed civilizations sing.
Zijn ongewoon verwarrend eenzaamme gelden slapen.|
                                                 |Its unusually confusing lonely moneys sleep.

# [det | adj noun] verb; 5 examples:
---
Hun lieve fietsen slapen.|
                         |Their nice bikes sleep.
De grote katten zien.|
                     |The big cats see.
Zijn bedroefde huizen probeeren.|
                                |Its sad houses try.
Die zenuwachtige katten werken.|
                               |Those nervous cats work.
Zijn verwarrende dagen zien.|
                            |His confusing days see.

# Sentence with object; 5 examples:
---
Zijn plaatsen zien Guus.|
                        |His lots see Guus.
Mijn haren zien mij.|
                    |My hairs see me.
Jij ziet u.|
           |You see you.
De jongens zien jou.|
                    |The boys see you.
Zijn mannen zien hun.|
                     |His men see him.

# Sentence with reflexive object; 5 examples:
---
Jullie zien je.|
               |You see yourselves.
Jij ziet je.|
            |You see yourself.
Hij ziet zich.|
              |He sees himself.
U ziet u.|
         |You see yourself.
Jij ziet je.|
            |You see yourself.

# [s np [vp v np]]; 5 examples:
---
Uw fietsen zien haar huizen.|
                            |Your bikes see her house.
Onze zaken lezen zijn tafels.|
                             |Our cases read its tables.
De ontwikkelingen lezen hun benen.|
                                  |The developments read their legs.
De oogen zien deze haren.|
                         |The eyes see this hair.
Zijn meisjes zien jouw tafels.|
                              |His girls see your table.

# [s n [vp v [vp:inf to v]]]; 5 examples:
---
Wij probeeren te werken.|
                        |We try to work.
Wij probeeren te probeeren.|
                           |We try to try.
Jullie probeeren te veroorzaaken.|
                                 |You try to cause.
Jullie probeeren te slapen.|
                           |You try to sleep.
Hij probeert te zingen.|
                       |He tries to sing.

# e.g. Jouw families probeeren mijn honden te zien.; 5 examples:
---
Onze groepen probeeren die kinderen te lezen.|
                                             |Our groups try to read that child.
Deze bedrijven probeeren die thuizen te lezen.|
                                              |These businesses try to read that home.
Haar zaken probeeren haar kinderen te veroorzaaken.|
                                                   |Her cases try to cause her child.
Zijn bedrijven probeeren mijn beschavingen te veroorzaaken.|
                                                           |Its businesses try to cause my civilizations.
Deze groepen probeeren zijn haren te lezen.|
                                           |These groups try to read its hair.

17
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
