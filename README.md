[![Clojars Project](https://img.shields.io/clojars/v/menard.svg)](https://clojars.org/menard)
[![Build Status](https://secure.travis-ci.org/ekoontz/menard.png?branch=master)](http://travis-ci.org/ekoontz/menard)
[![License](https://img.shields.io/badge/License-EPL%201.0-red.svg)](https://opensource.org/licenses/EPL-1.0)

# Menard

A Clojure library for generation and parsing of natural language expressions.

## Acknowledgements

### HPSG

Based on a linguistic theory called HPSG (Head Driven Phrase Structure Grammar). More about HPSG:

- http://hpsg.stanford.edu/
- https://en.wikipedia.org/wiki/Head-driven_phrase_structure_grammar

### [Fehringer 1999]

Dutch grammar and lexicon based on [Carole Fehringer, "A Reference Grammar of Dutch", Cambridge University Press, 1999](https://books.google.nl/books/about/A_Reference_Grammar_of_Dutch.html?id=hXZNkFqILp0C&redir_esc=y), referred to here as "F. &lt;section&gt;" or "F. pp. &lt;pages&gt;" in the source code.

### Verbix

Uses verb conjugations from Verbix: http://www.verbix.com 

## Demo

For the demo, a Dutch sentence is generated for each specification listed in
<a href="https://github.com/ekoontz/menard/blob/master/src/menard/nederlands/expressions.edn">expressions.edn</a>. 
Each expression is then translated into English, as shown in the output below:

```
user=> (do (load "menard/translate") (menard.translate/demo))
# intensifier adjective; 5 examples:
---
Erg vies.|
         |Very dirty.
Behoorlijk eng.|
               |Quite scary.
Eigenlijk opgevonden.|
                     |Actually excited.
Heel nieuwsgierig.|
                  |Very curious.
Heel eng.|
         |Very scary.

# det noun; 5 examples:
---
Negentien vogels.|
                 |Eighteen birds.
Deze leraar.|
            |This teacher.
Onze plaatsen.|
              |Our lots.
Eenzaamme verslaggevers.|
                        |Some lonely reporters.
Deze groepen.|
             |These groups.

# noun verb; 5 examples:
---
Haren lezen.|
            |Hairs read.
Landen zingen.|
              |Countries sing.
Hij uitgeleggt.|
               |He explains.
Druiven werken.|
               |Grapes work.
Benen uitgeleggen.|
                  |Legs explain.

# det noun | verb; 5 examples:
---
Deze baby komt.|
               |This baby comes.
Haar oplossing uitgeleggt.|
                          |Her solutions explain.
Die dames zien.|
               |Those ladies see.
Zijn feit werkt.|
                |Its fact works.
Deze fiets zingt.|
                 |This bike sings.

# det | adj noun; 5 examples:
---
Haar teleurgestelde dag.|
                        |Her disappointed day.
Jouw elf dames.|
               |Your eleven ladies.
Ons verdrietige gebeid.|
                       |Our sad area.
De ware kittens.|
                |The true kittens.
Hun ernstige baan.|
                  |Their serious job.

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
Geen ongewoon zelfverzekerd verdrietig sterk kind.|
                                                  |No unusually confident sad strong child.
Geen ongewoon verdacht sterk haar.|
                                  |No unusually suspicious strong hair.
Geen behoorlijk lief verdrietig verlegen sterk eigenwijs leven.|
                                                               |No quite nice sad shy strong stubborn life.
Geen echt groot waar verwarrend haar.|
                                     |No really big true confusing hair.
Een behoorlijk vies stom meisje.|
                                |A quite dirty stupid girl.

# [det [[intensifier adj] noun]]; 5 examples:
---
Mijn acht en zestig sterke ongeruste stomme haren.|
                                                  |My sixty eight strong anxious stupid hairs.
Zijn zes en achtig bedroefde tafels.|
                                    |Its eighty six sad tables.
Jouw ongewoon verdrietig verlegene ernstige verwarde eenzaamme verlegene slimme grote zelfverzekerde stomme been.|
                                                                                                                 |Your unusually sad shy serious confused lonely shy smart big confident stupid legs.
Haar vijf en deertig grote grote groepen.|
                                         |Her thirty five big big groups.
Haar eén en achtig inhalige families.|
                                     |Her eighty one greedy families.

# vier en twintig vogels; 5 examples:
---
Zeven en deertig vogels.|
                        |Some thirty seven birds.
Vier en twintig vogels.|
                       |Some twenty four birds.
Vier en vijftig vogels.|
                       |Some fifty four birds.
Negen en achtig vogels.|
                       |Eighty nine birds.
Zes en veertig vogels.|
                      |Some forty six birds.

# kleine vogel; 5 examples:
---
Ongerust leven.|
               |His life.
Verwarde baan.|
              |The job.
Opgevondene groep.|
                  |No group.
Inhalige baby.|
              |Her baby.
Zelfverzekerd gebeid.|
                     |Her area.

# vier vogels; 5 examples:
---
Vier jongens.|
             |These boys.
Deertig kittens.|
                |Their kittens.
Zestig heren.|
             |Its men.
Veertien jongens.|
                 |His boys.
Vijf fietsen.|
             |No bikes.

# vier kliene vogels; 5 examples:
---
Achttien opgevondene katten.|
                            |Her cats.
Negentien opgevondene fietsen.|
                              |His bikes.
Eén verdrietige oplossing.|
                          |Your solution.
Teen zelfverzekerde banen.|
                          |Your jobs.
Negen ernstige thuizen.|
                       |Some homes.

# de vier kliene vogels; 5 examples:
---
Onze zeven ware kittens.|
                        |Our seven true kittens.
Uw vijftig verwarrende leraren.|
                               |Your fifty confusing teachers.
Hun nul vieze gelden.|
                     |Their zero dirty moneys.
De drie oude zaken.|
                   |The three old cases.
Hun twintig oude mannen.|
                        |Their twenty old men.

# e.g. 'de vier en twintig kleine vogels'; 5 examples:
---
De vijf en achtig sterke zaken.|
                               |The eighty five strong cases.
Zijn negen en zestig zelfverzekerde druiven.|
                                            |His sixty nine confident grapes.
De vier en veertig nieuwsgierige jassen.|
                                        |The forty four curious coats.
Mijn vijf en deertig grote jongens.|
                                   |My thirty five big boys.
Die zes en negentig verwarrende haren.|
                                      |Those ninety six confusing hairs.

# 'De heel sterk slimme vrouwen zingen'.; 1 example:
---
Haar heel sterk slimme vrouwen zingen.|
                                      |Her very strong smart women sing.

# [det [intensifier adj | adj noun]] verb; 5 examples:
---
Die vier en zestig verwarde opstanden kunnen.|
                                             |Those sixty four confused revolts can.
Mijn eén en zestig grote opgevondene levens zien.|
                                                 |My sixty one big excited lives see.
Jouw ongewoon waar vieze nieuwsgierige ogen optredt.|
                                                    |Your unusually true dirty curious eyes perform.
Zijn vier en zestig enge oude bedroefde slimme bedrijven optreden.|
                                                                  |His sixty four scary old sad smart businesses perform.
Zijn vier en vijftig kleine vrouwen bestrijden.|
                                               |His fifty four small women overcome.

# [det | adj noun] verb; 5 examples:
---
Die twee landen optreden.|
                         |Those two countries perform.
Hun twalf zaken kunnen.|
                       |Their twelve cases can.
Vijf eenzaamheden uitgeleggen.|
                              |Five lonelinesses explain.
Negentig opstanden komen.|
                         |Some ninety revolts come.
Achttien jassen veroorzaaken.|
                             |Some eighteen coats cause.

# Sentence with object; 5 examples:
---
Eenzaamheden zien gebeiden.|
                           |Lonelinesses see areas.
Families zien katten.|
                     |Families see cats.
Moeders zien heren.|
                   |Mothers see men.
Wij zien haar.|
              |We see her.
We zien opleidingen.|
                    |We see educations.

# Sentence with reflexive object; 5 examples:
---
Ik zie me.|
          |I see myself.
We zien ons.|
            |We see ourselves.
U ziet u.|
         |You see yourself.
Ik zie me.|
          |I see myself.
Wij zien ons.|
             |We see ourselves.

# [s np [vp v np]]; 5 examples:
---
Die kitten ziet jongens.|
                        |That kitten sees boys.
Slimme vrouwen uitgeleggen Saskia.|
                                  |Some smart women explain Saskia.
Dertien fietsen bestrijden huizen.|
                                  |Some thirteen bikes overcome houses.
Slimme honden zien jongens.|
                           |Smart dogs see boys.
Eenzaamme overheden bestrijden mij.|
                                   |Some lonely governments overcome me.

# [s n [vp-modal-te v [vp-te:inf to v]]]; 5 examples:
---
Heren proberen te komen.|
                        |Gentlemen try to come.
Oplossingen proberen te bestrijden.|
                                   |Solutions try to overcome.
Vrouwen proberen te bestrijden.|
                               |Women try to overcome.
Huizen proberen te bestrijden.|
                              |Houses try to overcome.
Jassen proberen te optreden.|
                            |Coats try to perform.

# modals+infinitive; 5 examples:
---
De eenzaamme lieve oplossing probeert Corona te optreden.|
                                                         |The lonely nice solution tries to perform Corona.
Onze verdrietige jongen probeert maanden te zien.|
                                                 |Our sad boy tries to see months.
Hun achtig overheden proberen thuizen te zien.|
                                              |Their eighty governments try to see homes.
Jouw oude verdrietige lieve stomme verlegene voorstelling probeert jassen te veroorzaaken.|
                                                                                          |Your old sad nice stupid shy performances try to cause coats.
Wat vijftig zenuwachtige druiven proberen baby's te optreden.|
                                                             |Some fifty nervous grapes try to perform babies.

# using 'kunnen'; 5 examples:
---
Katten kunnen bestrijden.|
                         |Cats can overcome.
Fietsen kunnen komen.|
                     |Bikes can come.
Hij kan slapen.|
               |He can sleep.
Groepen kunnen bestrijden.|
                          |Groups can overcome.
Opstanden kunnen bestrijden.|
                            |Revolts can overcome.

# corona sentence from de Krant van de Gemente van Amsterdam; 1 example:
---
Corona moeten wij samen bestrijden.|
                                   |We must overcome Corona together.

# Generalization of the previous:; 5 examples:
---
Maanden moeten vrouwen samen lezen.|
                                   |Women must read months together.
Katten moeten stoelen samen uitgeleggen.|
                                        |Chairs must explain cats together.
Vrouwen moeten kinderen samen optreden.|
                                       |Children must perform women together.
Corona moeten mannen samen veroorzaaken.|
                                        |Men must cause Corona together.
Meisjes moeten wij samen uitgeleggen.|
                                     |We must explain girls together.

# e.g. 'de vier en twintig kleine vogels slapen'; 5 examples:
---
Deze drie en achtig gierige boeken werken.|
                                          |These eighty three stingy books work.
Uw negen en vijftig grote verslaggevers optreden.|
                                                 |Your fifty nine big reporters perform.
Zijn drie en twintig ernstige verslaggevers moeten.|
                                                   |Its twenty three serious reporters must.
Die vier en zestig teleurgestelde leraren bestrijden.|
                                                     |Those sixty four disappointed teachers overcome.
Jouw vijf en zestig zenuwachtige honden veroorzaaken.|
                                                     |Your sixty five nervous dogs cause.

28
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.

# Name

The story [Pierre Menard, autor del Quijote](https://en.wikipedia.org/wiki/Pierre_Menard,_Author_of_the_Quixote),
by Jorge Luis Borges, tells of a writer Pierre Menard who attempted to
rewrite Cervantes' Don Quixote, but not simply by copying it
letter-by-letter from the original text, but rather by somehow
internalizing the mental state of Cervantes that is necessary to write
it, and then writing it _de novo_ from that interior mental state.  I
see this as reminicent of this project's attempt to generate natural
language expressions by encoding semantic representations and then
using grammar rules and lexical entries that are able to generate the
expressions from the semantic representations.

Initially, I named this project [babel](https://github.com/ekoontz/babel), after another of Borges'
stories, [The Library of Babel](https://en.wikipedia.org/wiki/The_Library_of_Babel), but this
name is already well-known [Javascript tool](https://babeljs.io/). I
later rewrote and simplified the same ideas into a new github
repository, calling it 'babylon', but then discovered that this name
was also already well-known Javascript [software project](https://www.babylonjs.com/), and so now I have arrived at the
current name, by renaming this current repository as 'menard'.

I've found that there is another project called
[pierre-menard](https://github.com/hraberg/pierre-menard), also as it
happens written in Clojure, that is now archived with the last commit
being 2012, so I think it is safe to reappropriate this name for my
own project now in 2020.
