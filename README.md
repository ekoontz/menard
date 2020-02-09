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
nREPL server started on port 63640 on host 127.0.0.1 - nrepl://127.0.0.1:63640
REPL-y 0.4.3, nREPL 0.6.0
Clojure 1.10.1
OpenJDK 64-Bit Server VM 12.0.1+12
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (load "babylon/translate")
nil
user=> (babylon.translate/demo)
# intensifier adjective; 1 example:
---
Behoorlijk sterk.|
                 |Quite strong.

# det noun; 1 example:
---
Deze gelden.|
            |These moneys.

# noun verb; 1 example:
---
Wij slapen.|
           |We sleep.

# det noun | verb; 1 example:
---
De ontwikkelingen veroorzaaken.|
                               |The developments cause.

# det | adj noun; 1 example:
---
De nieuwsgierige handen.|
                        |The curious hands.

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
Onze behoorlijk ernstig ernstige ontwikkelingen.|
                                                |Our quite serious serious developments.

# 'De heel sterk slimme vrouen zingen'.; 1 example:
---
De heel sterk slimme vrouwen zingen.|
                                    |The very strong smart women sing.

# [det [intensifier adj | adj noun]] verb; 10 examples:
---
Haar erg sterk zelfverzekerdee gebeiden zien.|
                                             |Her very strong confident areas see.
Zijn erg ernstig kleine maanden slapen.|
                                       |His very serious small months sleep.
Hun ongewoon opgevonden oude fietsen zien.|
                                          |Their unusually excited old bikes see.
Zijn ongewoon verdrietig kleine levens werken.|
                                              |Its unusually sad small lives work.
Hun erg oud stomme plaatsen zien.|
                                 |Their very old stupid lots see.
Onze eigenlijk slim verwarde kinderen lezen.|
                                            |Our actually smart confused children read.
Mijn erg bedroefd zelfverzekerdee katten zingen.|
                                                |My very sad confident cats sing.
De heel nieuwsgierig opgevondene heren lezen.|
                                             |The very curious excited men read.
Mijn erg sterk lieve kinderen lezen.|
                                    |My very strong nice children read.
Die erg zelfverzekerde grote beschavingen zingen.|
                                                 |Those very confident big civilizations sing.

# [det | adj noun] verb; 10 examples:
---
Zijn ernstige oplossingen werken.|
                                 |His serious solutions work.
Onze ongeruste druiven zingen.|
                              |Our anxious grapes sing.
De eenzaamme thuizen slapen.|
                            |The lonely homes sleep.
De oude handen lezen.|
                     |The old hands read.
Haar bedroefde landen zien.|
                           |Her sad countries see.
Hun enge dames veroorzaaken.|
                            |Their scary ladies cause.
Uw verwarrende kittens werken.|
                              |Your confusing kittens work.
De verwarde huizen zingen.|
                          |The confused houses sing.
Zijn oude fietsen zingen.|
                         |His old bikes sing.
Jouw verlegene haren zingen.|
                            |Your shy hairs sing.

# Sentence with object; 10 examples:
---
Wij zien haar.|
              |We see her.
Jullie zien hun.|
                |You see him.
Het ziet hem.|
             |It sees him.
Wij zien Guus.|
              |We see Guus.
Jullie zien u.|
              |You see you.
Jullie zien mij.|
                |You see me.
Jullie zien jou.|
                |You see you.
Het ziet hen.|
             |It sees him.
U ziet hun.|
           |You see him.
Mijn dames zien Guus.|
                     |My ladies see Guus.

# Sentence with reflexive object; 10 examples:
---
Zij ziet zich.|
              |She sees herself.
Zij zien zich.|
              |They see themselves.
Jij ziet je.|
            |You see yourself.
U ziet u.|
         |You see yourselves.
U ziet u.|
         |You see yourself.
U ziet u.|
         |You see yourself.
Jullie zien je.|
               |You see yourselves.
Jullie zien je.|
               |You see yourselves.
U ziet u.|
         |You see yourself.
Saskia ziet zich.|
                 |Saskia sees herself.

# e.g.: [np een kat] [vp v ziet [np de muis]]; 10 examples:
---
Onze moeders lezen zijn benen.|
                              |Our mothers read its legs.
Hun katten veroorzaaken zijn zaken.|
                                   |Their cats cause its cases.
Uw fietsen veroorzaaken haar zaken.|
                                   |Your bikes cause her cases.
Mijn levens veroorzaaken zijn groepen.|
                                      |My lives cause its groups.
Hun ontwikkelingen zien zijn gebeiden.|
                                      |Their developments see its areas.
De haren veroorzaaken hun gebeiden.|
                                   |The hairs cause their area.
Haar heren lezen hun dames.|
                           |Men read their lady.
De dagen zien deze dagen.|
                         |The days see these days.
Deze thuizen lezen zijn zaken.|
                              |These homes read its cases.
Zijn mannen veroorzaaken onze gelden.|
                                     |Men cause our money.

15
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
