[![Clojars Project](https://img.shields.io/clojars/v/menard.svg)](https://clojars.org/menard)
[![Build Status](https://secure.travis-ci.org/ekoontz/menard.png?branch=master)](http://travis-ci.org/ekoontz/menard)
[![License](https://img.shields.io/badge/License-EPL%201.0-red.svg)](https://opensource.org/licenses/EPL-1.0)
[![Run on Repl.it](https://repl.it/badge/github/ekoontz/menard)](https://repl.it/github/ekoontz/menard)

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

For the demo, a Dutch sentence and a *semantically-equivalent** English sentence. First the Dutch sentence is generated (for each specification listed in
<a href="https://github.com/ekoontz/menard/blob/master/src/menard/nederlands/expressions.edn">expressions.edn</a>), then, the semantics
of this Dutch sentence are used to generate an English sentence with the same* semantics.

\* Approximately the same semantics, modulo various bugs and misunderstandings on my part.

```
$ echo "(load \"menard/translate\")(menard.translate/demo)" \
  | lein repl
```

The output will look like [this example](demo.txt), although you'll get your own, uniquely-generated set of sentences.

# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.

# Name

The story [Pierre Menard, autor del Quijote](https://en.wikipedia.org/wiki/Pierre_Menard,_Author_of_the_Quixote),
by Jorge Luis Borges, tells of a writer, Pierre Menard, who attempted to
rewrite Cervantes' Don Quixote, not simply by copying it
letter-by-letter from the original text, but rather by somehow
reproducing the mental and experiential state of Cervantes that was necessary to write
it, and then writing it _de novo_ from that state.  I
see this as reminicent of this project's attempt to generate natural
language expressions by encoding semantic representations and then
using grammar rules and lexical entries that are able to generate the
expressions from the semantic representations.

Initially, I named this project [babel](https://github.com/ekoontz/babel), after another of Borges'
stories, [The Library of Babel](https://en.wikipedia.org/wiki/The_Library_of_Babel), but this
name is already taken by a well-known [Javascript tool](https://babeljs.io/). I
later rewrote and simplified the same ideas in my 'babel' project into a new github
repository, calling it 'babylon', before discovering that this name
was also already a well-known Javascript [software project](https://www.babylonjs.com/). Now I have arrived at the
current name, by renaming the 'babylon' repository to 'menard'.

I've found that there is another project called
[pierre-menard](https://github.com/hraberg/pierre-menard), also as it
happens written in Clojure, that is now archived with the last commit
being 2012, so I think it is safe to reappropriate this name for my
own project now in 2020.

# Inspirations

Besides the above-mentioned linguistics and Borges references mentioned above,
I am including some links below which I think are interesting and
related to the ideas I am exploring here.

- [500'000€ Prize for Compressing Human Knowledge](http://prize.hutter1.net/index.htm)
- [The Library of Babel](https://libraryofbabel.info/)
- [Abstract Meaning Representation](https://amr.isi.edu/)

