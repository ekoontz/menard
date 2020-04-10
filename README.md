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

Dutch grammar and lexicon based on [Carole Fehringer, "A Reference Grammar of Dutch", Cambridge University Press, 1999](https://books.google.nl/books/about/A_Reference_Grammar_of_Dutch.html?id=hXZNkFqILp0C&redir_esc=y), referred to here as "F. <section>" or "F. pp. <pages>" in the source code.

### Verbix

Uses verb conjugations from Verbix: http://www.verbix.com 

## Demo

For the demo, a Dutch sentence is generated for each specification listed in
<a href="https://github.com/ekoontz/babylon/blob/master/src/babylon/nederlands/expressions.edn">expressions.edn</a>. 
Each expression is then translated into English, as shown in the output below:

```
% lein repl
nREPL server started on port 56066 on host 127.0.0.1 - nrepl://127.0.0.1:56066
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
Ongewoon eng.|
             |Unusually scary.

# det noun; 1 example:
---
Onze stoelen.|
             |Our chairs.

# noun verb; 1 example:
---
U zingt.|
        |You sing.

# det noun | verb; 1 example:
---
Zijn druif leest.|
                 |His grape reads.

# det | adj noun; 1 example:
---
Haar ongeruste boeken.|
                      |Her anxious books.

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
De erg nieuwsgierig ernstige kittens.|
                                     |The very curious serious kittens.

# 'De heel sterk slimme vrouen zingen'.; 1 example:
---
De heel sterk slimme vrouwen zingen.|
                                    |The very strong smart women sing.

# [det [intensifier adj | adj noun]] verb; 5 examples:
---
Een echt ernstig ware plaats komt.|
                                  |A really serious true lot comes.
Onze behoorlijk slim zelfverzekerde moeder komt.|
                                                |Our quite smart confident mother comes.
Uw heel klein verdrietige man werkt.|
                                    |Your very small sad man works.
Hun eigenlijk lief zelfverzekerde handen slapen.|
                                                |Their actually nice confident hands sleep.
Deze erg stom zelfverzekerde landen veroorzaaken.|
                                                 |These very stupid confident countries cause.

# [det | adj noun] verb; 5 examples:
---
Haar verwarrende fietsen komen.|
                               |Her confusing bikes come.
Deze ernstige zaken komen.|
                          |These serious cases come.
Zijn zelfverzekerde moeder werkt.|
                                 |Its confident mother works.
Een waar feit slaapt.|
                     |A true fact sleeps.
De sterke ogen komen.|
                     |The strong eyes come.

# Sentence with object; 5 examples:
---
Zij zien Guus.|
              |They see Guus.
Guus ziet hun.|
              |Guus sees them.
Ik zie hen.|
           |I see them.
Jij ziet hen.|
             |You see them.
Jij ziet haar.|
              |You see her.

# Sentence with reflexive object; 5 examples:
---
Ik zie me.|
          |I see myself.
U ziet u.|
         |You see yourselves.
U ziet u.|
         |You see yourselves.
Jullie zien je.|
               |You see yourselves.
Zij zien zich.|
              |They see themselves.

# [s np [vp v np]]; 5 examples:
---
Mijn oplossing leest jouw kitten.|
                                 |My solution reads your kitten.
Zijn huis ziet deze hond.|
                         |His house sees this dog.
Onze tafel ziet de jongen.|
                          |Our table sees the boy.
Haar geld veroorzaakt uw stoelen.|
                                 |Her money causes your chairs.
Zijn kittens zien jouw jongen.|
                              |Its kittens see your boy.

# [s n [vp v [vp:inf to v]]]; 5 examples:
---
Jij probeert te zien.|
                     |You try to see.
Het probeert te slapen.|
                       |It tries to sleep.
Ik probeer te slapen.|
                     |I try to sleep.
Wij proberen te werken.|
                       |We try to work.
Wij proberen te komen.|
                      |We try to come.

# 'Haar heel verdrietig zelfverzekerde beschaving probeert mijn heel verdrietig overheid te veroorzaaken.'/'Her very sad confident civilization tries to cause my very sad government.'; 5 examples:
---
Hun erg sterk slimme man probeert uw heel zelfverzekerd dame te zien.|
                                                                     |Their very strong smart man tries to see your very confident lady.
Hun heel opgevonden ernstige moeders proberen uw eigenlijk oud thuizen te veroorzaaken.|
                                                                                       |Their very excited serious mothers try to cause your actually old homes.
Deze ongewoon verward eenzaamme beschavingen proberen zijn eigenlijk opgevonden kind te veroorzaaken.|
                                                                                                     |These unusually confused lonely civilizations try to cause his actually excited child.
Onze echt klein zenuwachtige levens proberen mijn ongewoon verwarrend heer te veroorzaaken.|
                                                                                           |Our really small nervous lives try to cause my unusually confusing man.
Hun behoorlijk vies lieve dagen proberen de erg oud oplossingen te zien.|
                                                                        |Their quite dirty nice days try to see the very old solutions.

---
Wij kunnen zingen.|
                  |We can sing.

18
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
