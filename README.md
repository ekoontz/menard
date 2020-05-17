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
OpenJDK 64-Bit Server VM warning: Options -Xverify:none and -noverify were deprecated in JDK 13 and will likely be removed in a future release.
nREPL server started on port 53777 on host 127.0.0.1 - nrepl://127.0.0.1:53777
REPL-y 0.4.4, nREPL 0.6.0
Clojure 1.10.1
OpenJDK 64-Bit Server VM 14.0.1+7
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (do (load "babylon")(babylon.translate/demo))
# intensifier adjective; 5 examples:
---
Behoorlijk eigenwijs.|
                     |Quite stubborn.
Erg verdrietig.|
               |Very sad.
Behoorlijk zenuwachtig.|
                       |Quite nervous.
Behoorlijk sterk.|
                 |Quite strong.
Ongewoon inhalig.|
                 |Unusually greedy.

# det noun; 5 examples:
---
Twalf ontwikkelingen.|
                     |Twelve developments.
Die kitten.|
           |That kitten.
Die groep.|
          |That group.
Uw katten.|
          |Your cats.
Mijn druiven.|
             |My grapes.

# noun verb; 5 examples:
---
Ontwikkelingen slapen.|
                      |Developments sleep.
Corona veroorzaakt.|
                   |Corona causes.
Ontwikkelingen zien.|
                    |Developments see.
Corona werkt.|
             |Corona works.
Jassen kunnen.|
              |Coats can.

# det noun | verb; 5 examples:
---
Onze dames moeten.|
                  |Our ladies must.
Acht overheden zingen.|
                      |Eight governments sing.
Deze heren lezen.|
                 |These gentlemen read.
Dat huis bestrijdt.|
                   |That house overcomes.
Hun boek uitgeleggt.|
                    |Their book explains.

# det | adj noun; 5 examples:
---
Mijn kleine beschaving.|
                       |My small civilization.
Zes stomme handen.|
                  |Six stupid hands.
De enge jassen.|
               |The scary coats.
Jouw verwarrende feitten.|
                         |Your confusing facts.
Geen slimme hand.|
                 |No smart hand.

# 'een huis'; 1 example:
---
Een huis.|
         |A house.

# 'de grote boeken'; 1 example:
---
Zeven grote boeken.|
                   |Seven big books.

# 'een heel klein druif'; 1 example:
---
Een heel klein druif.|
                     |A very small grape.

# [det [[intensifier adj] noun]]; 5 examples:
---
Een echt teleurgesteld ernstig bedrijf.|
                                       |A really disappointed serious business.
Geen eigenlijk bedroefd bedroefd haar.|
                                      |No actually sad sad hair.
Een ongewoon stom lief vrouw.|
                             |An unusually stupid nice woman.
Eén echt stom bedroefd vrouw.|
                             |One really stupid sad woman.
Een behoorlijk gierig opgevonden bedrijf.|
                                         |A quite stingy excited business.

# [det [[intensifier adj] noun]]; 5 examples:
---
Haar behoorlijk ongeruste zelfverzekerde tafels.|
                                                |Her quite anxious confident tables.
Twee ongewoon klein eigenwijze benen.|
                                     |Two unusually small stubborn legs.
Zijn behoorlijk vieze verwarde banen.|
                                     |Its quite dirty confused jobs.
Haar heel stomme verdrietige been.|
                                  |Her very stupid sad leg.
Zeven eigenlijk groot sterke levens.|
                                    |Seven actually big strong lives.

# 'De heel sterk slimme vrouen zingen'.; 1 example:
---
Negen erg sterk slimme vrouwen zingen.|
                                      |Nine very strong smart women sing.

# [det [intensifier adj | adj noun]] verb; 5 examples:
---
De eigenlijk enge grote groepen lezen.|
                                      |The actually scary big groups read.
Een behoorlijk nieuwsgierig zenuwachtige stoel kan.|
                                                   |A quite curious nervous chair can.
Drie echt groot ware feitten uitgeleggen.|
                                         |Three really big true facts explain.
Teen eigenlijk inhalig inhalige bedrijven veroorzaaken.|
                                                       |Ten actually greedy greedy businesses cause.
Die echt eenzaamme slimme familie kan.|
                                      |That really lonely smart family can.

# [det | adj noun] verb; 5 examples:
---
Zijn verwarde been werkt.|
                         |Its confused leg works.
Deze inhalige dagen werken.|
                           |These greedy days work.
Hun opgevondene moeders komen.|
                              |Their excited mothers come.
Vijf sterke jongens werken.|
                           |Five strong boys work.
Twalf verlegene dagen bestrijden.|
                                 |Twelve shy days overcome.

# Sentence with object; 5 examples:
---
Vrouwen zien die.|
                 |Women see them.
Honden zien heren.|
                  |Dogs see gentlemen.
Kittens zien oplossingen.|
                         |Kittens see solutions.
Ontwikkelingen zien landen.|
                           |Developments see countries.
Hij ziet druiven.|
                 |He sees grapes.

# Sentence with reflexive object; 5 examples:
---
Ik zie me.|
          |I see myself.
U ziet u.|
         |You see yourself.
We zien ons.|
            |We see ourselves.
Ik zie me.|
          |I see myself.
Jij ziet je.|
            |You see yourself.

# [s np [vp v np]]; 5 examples:
---
Mijn thuis uitgeleggt vijf huizen.|
                                  |My home explains five houses.
Acht gelden veroorzaaken zijn baby's.|
                                     |Eight moneys cause its babies.
Zijn eenzaamheid ziet mijn familie.|
                                   |Its loneliness sees my family.
Deze groep ziet negen oplossingen.|
                                  |This group sees nine solutions.
Uw families lezen het vrouw.|
                            |Your families read the woman.

# [s n [vp-modal-te v [vp-te:inf to v]]]; 5 examples:
---
Mannen proberen te bestrijden.|
                              |Men try to overcome.
Corona probeert te werken.|
                          |Corona tries to work.
Saskia probeert te uitgeleggen.|
                               |Saskia tries to explain.
Handen proberen te werken.|
                          |Hands try to work.
Guus probeert te lezen.|
                       |Guus tries to read.

# modals+infinitive; 5 examples:
---
Negen echt zelfverzekerd eenzaamme thuizen proberen die heel kleine eenzaamheid te bestrijden.|
                                                                                              |Nine really confident lonely homes try to overcome that very small loneliness.
Drie behoorlijk verdrietig opgevondene ogen proberen negen erg teleurgesteld fietsen te zien.|
                                                                                             |Three quite sad excited eyes try to see nine very disappointed bikes.
Deze heel eigenwijze grote groep probeert hun echt verlegene groep te zien.|
                                                                           |This very stubborn big group tries to see their really shy group.
Zijn eigenlijk inhalige nieuwsgierige levens proberen zijn ongewoon zelfverzekerde fiets te bestrijden.|
                                                                                                       |His actually greedy curious lives try to overcome his unusually confident bike.
Geen eigenlijk verdrietig nieuwsgierige man probeert mijn behoorlijk eenzaamme honden te uitgeleggen.|
                                                                                                     |No actually sad curious man tries to explain my quite lonely dogs.

# using 'kunnen'; 5 examples:
---
Ontwikkelingen kunnen komen.|
                            |Developments can come.
Jassen kunnen bestrijden.|
                         |Coats can overcome.
Kittens kunnen bestrijden.|
                          |Kittens can overcome.
We kunnen komen.|
                |We can come.
Landen kunnen lezen.|
                    |Countries can read.

# corona sentence from de Krant van de Gemente van Amsterdam; 1 example:
---
Corona moeten wij samen bestrijden.|
                                   |We must overcome Corona together.

# Generalization of the previous:; 5 examples:
---
Beschavingen moeten tafels samen lezen.|
                                       |Tables must read civilizations together.
Corona moeten banen samen veroorzaaken.|
                                       |Jobs must cause Corona together.
Groepen moeten zij samen zien.|
                              |They must see groups together.
Heren moeten plaatsen samen uitgeleggen.|
                                        |Lots must explain men together.
Groepen moeten dagen samen zien.|
                                |Days must see groups together.

21
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
