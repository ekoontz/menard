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
nREPL server started on port 56520 on host 127.0.0.1 - nrepl://127.0.0.1:56520
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
# intensifier adjective; 5 examples:
---
Echt opgevonden.|
                |Really excited.
Behoorlijk nieuwsgierig.|
                        |Quite curious.
Ongewoon eenzaam.|
                 |Unusually lonely.
Erg eigenwijs.|
              |Very stubborn.
Echt vies.|
          |Really dirty.

# det noun; 5 examples:
---
Jouw meisjes.|
             |Your girls.
Uw fiets.|
         |Your bike.
Onze ontwikkeling.|
                  |Our development.
Onze banen.|
           |Our jobs.
De stoel.|
         |The chair.

# noun verb; 5 examples:
---
Jullie komen.|
             |You come.
Jij werkt.|
          |You work.
Corona veroorzaakt.|
                   |Corona causes.
Zij veroorzaaken.|
                 |They cause.
Guus kan.|
         |Guus can.

# det noun | verb; 5 examples:
---
De dagen kunnen.|
                |The days can.
Deze katten komen.|
                  |These cats come.
Onze heren zien.|
                |Our gentlemen see.
Uw ontwikkelingen werken.|
                         |Your developments work.
Onze boeken zingen.|
                   |Our books sing.

# det | adj noun; 5 examples:
---
Deze vieze heren.|
                 |These dirty gentlemen.
Hun lieve huizen.|
                 |Their nice houses.
Haar zelfverzekerde bedrijven.|
                              |Her confident businesses.
Dit verlegene geld.|
                   |This shy money.
Geen teleurgesteld thuis.|
                         |No disappointed home.

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

# [det [[intensifier adj] noun]]; 5 examples:
---
Geen behoorlijk sterk ernstig vrouw.|
                                    |No quite strong serious woman.
Geen behoorlijk inhalig verward kind.|
                                     |No quite greedy confused child.
Geen ongewoon ernstig lief boek.|
                                |No unusually serious nice book.
Geen heel vies lief land.|
                         |No very dirty nice country.
Geen heel verwarrend ongerust geld.|
                                   |No very confusing anxious money.

# [det [[intensifier adj] noun]]; 5 examples:
---
De echt kleine verwarrende hand.|
                                |The really small confusing hand.
Een ongewoon lief vies feit.|
                            |An unusually nice dirty fact.
Ons heel inhalige opgevondene haar.|
                                   |Our very greedy excited hair.
Hun ongewoon teleurgestelde ongeruste druif.|
                                            |Their unusually disappointed anxious grape.
Mijn echt enge stomme maanden.|
                              |My really scary stupid months.

# 'De heel sterk slimme vrouen zingen'.; 1 example:
---
Zijn heel sterke slimme vrouwen zingen.|
                                       |Its very strong smart women sing.

# [det [intensifier adj | adj noun]] verb; 5 examples:
---
Haar echt opgevondene sterke baby moet.|
                                       |Her really excited strong baby must.
Jouw erg grote enge gebeiden zien.|
                                  |Your very big scary areas see.
Deze ongewoon ongeruste verdrietige benen kunnen.|
                                                 |These unusually anxious sad legs can.
Uw echt oude zelfverzekerde land kan.|
                                     |Your really old confident country can.
Ons ongewoon gierige ongeruste gebeid slaapt.|
                                             |Our unusually stingy anxious area sleeps.

# [det | adj noun] verb; 5 examples:
---
Dit ernstige gebeid zingt.|
                          |This serious area sings.
De verlegene mannen zingen.|
                           |The shy men sing.
Uw vieze honden veroorzaaken.|
                             |Your dirty dogs cause.
Zijn stomme druif slaapt.|
                         |His stupid grape sleeps.
Zijn ernstige haren zingen.|
                           |Its serious hairs sing.

# Sentence with object; 5 examples:
---
Jij ziet haar.|
              |You see her.
Corona ziet hun.|
                |Corona sees them.
U ziet hem.|
           |You see him.
Hij ziet Guus.|
              |He sees Guus.
Het ziet u.|
           |It sees you.

# Sentence with reflexive object; 5 examples:
---
Wij zien ons.|
             |We see ourselves.
Ik zie me.|
          |I see myself.
Wij zien ons.|
             |We see ourselves.
Zij ziet zich.|
              |She sees herself.
Jullie zien je.|
               |You see yourselves.

# [s np [vp v np]]; 5 examples:
---
Haar zaak veroorzaakt zijn dagen.|
                                 |Her case causes its days.
Hun haren uitgeleggen zijn jongens.|
                                   |Their hairs explain its boys.
Hun fietsen bestrijden deze huizen.|
                                   |Their bikes overcome these houses.
Dat leven bestrijdt zijn meisje.|
                                |That life overcomes its girl.
De druiven lezen haar familie.|
                              |The grapes read her family.

# [s n [vp-modal-te v [vp-te:inf to v]]]; 5 examples:
---
Guus probeert te zingen.|
                        |Guus tries to sing.
Wij proberen te uitgeleggen.|
                            |We try to explain.
Hij probeert te slapen.|
                       |He tries to sleep.
Zij proberen te bestrijden.|
                           |They try to overcome.
Jij probeert te veroorzaaken.|
                             |You try to cause.

# modals+infinitive; 5 examples:
---
Jouw erg kleine ernstige bedrijf probeert hun ongewoon enge baby's te bestrijden.|
                                                                                 |Your very small serious business tries to overcome their unusually scary babies.
Zijn ongewoon zenuwachtige kleine fiets probeert haar behoorlijk eenzaamme baby's te lezen.|
                                                                                           |His unusually nervous small bike tries to read her quite lonely babies.
Uw heel lieve oude dames proberen uw behoorlijk eigenwijze moeder te veroorzaaken.|
                                                                                  |Your very nice old ladies try to cause your quite stubborn mother.
Zijn echt stomme nieuwsgierige banen proberen jouw ongewoon ernstige jongens te veroorzaaken.|
                                                                                             |His really stupid curious jobs try to cause your unusually serious boys.
Die echt nieuwsgierige grote mannen proberen zijn eigenlijk verwarrende ogen te uitgeleggen.|
                                                                                            |Those really curious big men try to explain his actually confusing eyes.

# using 'kunnen'; 5 examples:
---
Jullie kunnen uitgeleggen.|
                          |You can explain.
Hij kan zingen.|
               |He can sing.
Jij kunt zingen.|
                |You can sing.
We kunnen bestrijden.|
                     |We can overcome.
Zij kunnen zien.|
                |They can see.

# corona sentence from de Krant van de Gemente van Amsterdam; 1 example:
---
Corona moeten wij samen bestrijden.|
                                   |We must overcome Corona together.

# Generalization of the previous:; 5 examples:
---
Guus moeten wij samen zien.|
                           |We must see Guus together.
Jou moeten u samen uitgeleggen.|
                               |You must explain you together.
Corona moeten jullie samen uitgeleggen.|
                                       |You must explain Corona together.
Hem moeten zij samen zien.|
                          |They must see him together.
Mij moeten wij samen uitgeleggen.|
                                 |We must explain me together.

21
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
