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
nREPL server started on port 53682 on host 127.0.0.1 - nrepl://127.0.0.1:53682
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
Ongewoon ernstig.|
                 |Unusually serious.

# det noun; 1 example:
---
Deze hond.|
          |This dog.

# noun verb; 1 example:
---
Ik probeer.|
           |I try.

# det noun | verb; 1 example:
---
Mijn vrouwen veroorzaaken.|
                          |My women cause.

# det | adj noun; 1 example:
---
Zijn verlegene kat.|
                   |Its shy cat.

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
Die heel zenuwachtig verwarrende feitten.|
                                         |Those very nervous confusing facts.

# 'De heel sterk slimme vrouen zingen'.; 1 example:
---
De erg sterk slimme vrouwen zingen.|
                                   |The very strong smart women sing.

# [det [intensifier adj | adj noun]] verb; 5 examples:
---
Deze erg groot verdrietige kittens veroorzaaken.|
                                                |These very big sad kittens cause.
Zijn ongewoon opgevonden stomme plaats zingt.|
                                             |Its unusually excited stupid lot sings.
Uw ongewoon sterk slimme haren veroorzaaken.|
                                            |Your unusually strong smart hairs cause.
Zijn ongewoon zenuwachtig verwarrende plaats probeert.|
                                                      |Its unusually nervous confusing lot tries.
Uw ongewoon sterk nieuwsgierige banen proberen.|
                                               |Your unusually strong curious jobs try.

# [det | adj noun] verb; 5 examples:
---
Jouw grote boeken zien.|
                       |Your big books see.
Haar kleine beschavingen veroorzaaken.|
                                      |Her small civilizations cause.
Ons bedroefde huis zingt.|
                         |Our sad house sings.
Jouw sterke baby werkt.|
                       |Your strong baby works.
Een kleine moeder werkt.|
                        |A small mother works.

# Sentence with object; 5 examples:
---
U ziet die.|
           |You see them.
Hij ziet mij.|
             |He sees me.
Zij zien haar.|
              |They see her.
Jij ziet hen.|
             |You see them.
Zij zien u.|
           |They see you.

# Sentence with reflexive object; 5 examples:
---
U ziet u.|
         |You see yourselves.
Het ziet zich.|
              |It sees itself.
Zij zien zich.|
              |They see themselves.
Het ziet zich.|
              |It sees itself.
Ik zie me.|
          |I see myself.

# [s np [vp v np]]; 5 examples:
---
Uw gebeid ziet hun maand.|
                         |Your area sees their month.
De dames zien de dagen.|
                       |The ladies see the days.
Zijn jas veroorzaakt het feit.|
                              |Its coat causes the fact.
De overheden veroorzaaken hun familie.|
                                      |The governments cause their family.
Hun beschaving veroorzaakt zijn meisjes.|
                                        |Their civilization causes his girls.

# [s n [vp v [vp:inf to v]]]; 5 examples:
---
Zij proberen te proberen.|
                         |They try to try.
Zij proberen te zingen.|
                       |They try to sing.
Zij proberen te veroorzaaken.|
                             |They try to cause.
Hij probeert te slapen.|
                       |He tries to sleep.
Saskia probeert te zien.|
                        |Saskia tries to see.

# e.g. Jouw families probeeren mijn honden te zien.; 5 examples:
---
Een behoorlijk lief ongerust meisje probeert mijn teleurgestelde feitten te zien.|
                                                                                 |A quite nice anxious girl tries to see my disappointed facts.
Onze eigenlijk ongerust slimme zaken proberen hun nieuwsgierige been te lezen.|
                                                                              |Our actually anxious smart cases try to read their curious leg.
De erg verwarrend grote jassen proberen haar stomme familie te lezen.|
                                                                     |The very confusing big coats try to read her stupid family.
Zijn eigenlijk zelfverzekerd verdrietige kittens proberen die stomme man te veroorzaaken.|
                                                                                         |Its actually confident sad kittens try to cause that stupid man.
Zijn echt nieuwsgierig zelfverzekerde plaatsen proberen dit enge leven te lezen.|
                                                                                |Its really curious confident lots try to read this scary life.

17
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
