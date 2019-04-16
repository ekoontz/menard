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

Based on a linguistic theory called HPSG (Head Driven Phrase Structure Grammar). More about HPSG:

- http://hpsg.stanford.edu/
- https://en.wikipedia.org/wiki/Head-driven_phrase_structure_grammar

# Getting Started

```
% lein repl
Warning: implicit hook found: lein-environ.plugin/hooks
Hooks are deprecated and will be removed in a future version.
nREPL server started on port 61788 on host 127.0.0.1 - nrepl://127.0.0.1:61788
REPL-y 0.4.3, nREPL 0.6.0
Clojure 1.10.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_121-b13
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (load "babylon/english")
nil
user=> (babylon.english/demo)
Generation:
===

= transitive sentences =

Questions had been words.
Moneys will show studies.
Books were wanting to look.
You begin people.
Students are wanting to show.
It will walk to nights.
They tried to move.
Boys have got problems.
We were wanting to put.
Cats found eyes.

= reflexive sentences =

WARN  16 apr 2019 11:43:46,618 babylon.english: failed to generate: [s  . *[vp-aux  *were .[vp-modal-1  *needing .[infinitive  *to .[vp-modal-1  *need .[infinitive  *to .:top]]]]]] with spec:{:cat :verb, :reflexive true, :root :top, :canonical :top, :agr :top, :subcat {:2 (), :1 {:top :top, :subcat (), :sem :top, :mod :top, :agr :top}}, :infl :base, :sem {:subj :top}}; at path:(:head :comp :comp :comp :comp :comp);e=class clojure.lang.ExceptionInfo
WARN  16 apr 2019 11:44:06,954 babylon.english: failed to generate: [s  . *[vp-aux  *am .[vp-modal-1  *wanting .[infinitive  *to .[vp-modal-2  *can .[vp  *move ._]]]]]] with spec:{:subcat (), :mod :top, :case :acc, :sem {:obj :top}, :cat :prep, :reflexive true, :agr :top}; at path:(:head :comp :comp :comp :comp :comp);e=class clojure.lang.ExceptionInfo
He tried to must give himself.
I will let on myself.
It makes itself.
I have started at myself.
WARN  16 apr 2019 11:44:37,840 babylon.english: failed to generate: [s  . *[vp-aux  *will .[vp-modal-1  *want .[infinitive  *to .[vp-modal-2  *can .[vp  *make ._]]]]]] with spec:{:subcat (), :mod :top, :case :acc, :sem {:obj :top}, :cat :prep, :reflexive true, :agr :top}; at path:(:head :comp :comp :comp :comp :comp);e=class clojure.lang.ExceptionInfo
I will seem myself.
It kept to itself.
WARN  16 apr 2019 11:45:00,508 babylon.english: failed to generate: [s  . *[vp-aux  *had .[vp-modal-1  *needed .[infinitive  *to .[vp-modal-1  *want .[infinitive  *to .:top]]]]]] with spec:{:cat :verb, :reflexive true, :root :top, :canonical :top, :agr :top, :subcat {:2 (), :1 {:top :top, :subcat (), :sem :top, :mod :top, :agr :top}}, :infl :base, :sem {:subj :top}}; at path:(:head :comp :comp :comp :comp :comp);e=class clojure.lang.ExceptionInfo
Points would call to themselves.
Years need to want to make themselves.
She has needed to find herself.
He will like himself.

= Interrogative sentences =

Would parts do?
Are students putting?
Are lots being?
Have eyes gone?
Have parts slept?
Were jobs being?
Were you looking?
Were schools giving?
Were days liking?
Have boys showed?

Parsing:
===

WARN  16 apr 2019 11:45:48,413 babylon.english: failed to generate: [nbar2  *rights .[s-slash  . *[vp-aux-slash  *have .:toped]]] with spec:{:aux false, :cat :verb, :infl :past-participle, :subcat {:1 {:subcat (), :sem :top, :agr :top}, :2 {:sem {:pred :right, :ref :top}}}, :agr :top, :modal :infinitive, :sem {:obj {:pred :right, :ref :top}, :ref :top, :subj :top, :aspect :perfect, :tense :past, :mood :decl}, :root :top, :canonical :top}; at path:(:comp :head :comp);e=class clojure.lang.ExceptionInfo
[nbar4  *rights .[comp1  *that .[s-slash  .facts *thought]]], [nbar4  *rights .[comp1  *that .[s-slash  .facts *thought]]]
[nbar2  *dogs .[s-slash  .numbers *find]], [nbar2  *dogs .[s-slash  .numbers *find]], [nbar2  *dogs .[s-slash  .numbers *find]], [nbar2  *dogs .[s-slash  .numbers *find]]
[s-interog  *were .[s-comp  .systems *turning]], [s-interog  *were .[s-comp  .systems *turning]]
WARN  16 apr 2019 11:45:57,466 babylon.english: failed to generate: [nbar2  *eyes .[s-slash  . *[vp-aux-slash  *were .:topping]]] with spec:{:aux false, :cat :verb, :infl :gerund, :subcat {:1 {:subcat (), :sem :top, :agr :top}, :2 {:sem {:pred :eye, :ref :top}}}, :agr :top, :modal :infinitive, :sem {:obj {:pred :eye, :ref :top}, :ref :top, :subj :top, :aspect :progressive, :tense :past, :mood :decl}, :root :top, :canonical :top}; at path:(:comp :head :comp);e=class clojure.lang.ExceptionInfo
WARN  16 apr 2019 11:46:05,101 babylon.english: failed to generate: [nbar4  *cases .[comp1  *that .[vp-aux-slash  *had .[vp-modal-1  *wanted .[infinitive  *to .[vp-modal-1  *:top ._]]]]]] with spec:{:aux false, :cat :verb, :slash false, :phrasal false, :infl :base, :reflexive :top, :subcat {:1 {:subcat {:2 (), :1 {:cat :noun, :case :acc, :reflexive false, :top :top, :agr :top, :sem :top}}, :agr :top, :slash false, :phrasal true, :infl :infinitive, :cat :verb, :reflexive :top}, :2 ()}, :agr :top, :modal :infinitive, :sem {:ref :top, :pred :case, :subj :top, :mood :decl}, :root :top, :canonical :top}; at path:(:comp :comp :comp :comp :comp :head);e=class clojure.lang.ExceptionInfo
[s-comp  .[nbar2  *questions .[s-slash  .governments *are]] *being], [s-comp  .[nbar2  *questions .[s-slash  .governments *are]] *being]
[s  .questions *used], [s-comp  .questions *used], [s-slash  .questions *used], [s-slash  .questions *used]
[s  .Antonio *[vp-aux  *was .[vp  *seeming .[pp  *to .himself]]]], [s  .Antonio *[vp-aux  *was .[vp  *seeming .[pp  *to .himself]]]]
WARN  16 apr 2019 11:46:16,187 babylon.lexiconfn: (matching-lexemes 'have'): both regular inflections (38) and exceptions (27).
WARN  16 apr 2019 11:46:16,187 babylon.lexiconfn: (matching-lexemes 'have'): both regular inflections (38) and exceptions (27).
[s  .days *[vp-aux  *have .had]], [s-slash  .days *[vp-aux-slash  *have .had]], [s-slash  .days *[vp-aux-slash  *have .had]], [s-slash  .days *[vp-aux-slash  *have .had]], [s-slash  .days *[vp-aux-slash  *have .had]]
[s  .groups *[vp-aux  *are .[vp-modal-1  *trying .[infinitive  *to .take]]]], [s  .groups *[vp-aux  *are .[vp-modal-1  *trying .[infinitive  *to .take]]]]
[s  .moneys *[vp-aux  *are .walking]], [s-slash  .moneys *[vp-aux-slash  *are .walking]], [s-slash  .moneys *[vp-aux-slash  *are .walking]], [s-slash  .moneys *[vp-aux-slash  *are .walking]], [s-slash  .moneys *[vp-aux-slash  *are .walking]]
[np  .the *story]
10
user=>
```

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
