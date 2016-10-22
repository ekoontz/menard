[![Clojars Project](https://img.shields.io/clojars/v/babel.svg)](https://clojars.org/babel)
[![Build Status](https://secure.travis-ci.org/ekoontz/babel.png?branch=master)](http://travis-ci.org/ekoontz/babel)

# babel

A Clojure library for generation and parsing of natural language expressions.

```clojure
user=> (require 'babel.english)
nil

;; generate a random expression in English
user=> (-> :top babel.english/generate babel.english/morph)
"your womens' new cities will lose me"

;; generate an English sentence about dogs eating
user=> (-> {:synsem {:sem {:pred :eat :subj {:pred :cane}}}} babel.english/generate babel.english/morph)
"your first student's new dogs used to eat a small music's pizza"

;; generate a random expression in Italian
user=> (require 'babel.italiano)
(-> :top babel.italiano/generate babel.italiano/morph)
"qualche neonato cittadino bene non bene non la sua"

;; generate an Italian sentence about cats drinking
(-> {:synsem {:sem {:pred :drink :subj {:pred :cat}}}} babel.italiano/generate babel.italiano/morph)
"in delle brutto isole uno corto cane berrà la tua ensalata"
user> 
```

```shell
demo.sh
```

runs
[babel.english.demo/demo](https://github.com/ekoontz/babel/blob/master/src/babel/english/demo.cljc)
and will demonstrate some of the library's abilities to generate
English expressions. [Here is the output of a sample run of
demo.sh](https://gist.github.com/ekoontz/57c332d85ccf47503666c72fe241cb14)

```shell
lein run -m babel.english/sentences 100
```

runs [babel.english/sentences](https://github.com/ekoontz/babel/blob/master/src/babel/english.cljc#L76) and will generate 100 random English sentences. [Here is the output of a sample run](https://gist.github.com/ekoontz/999b640014578b408437b97d5fdc7221).


```shell
lein test
```

will show a more in-depth verification of the library's behavior.

# HPSG (Head Driven Phrase Structure Grammar)

Babel is based on a linguistic theory called HPSG. For more details please see:

- http://hpsg.stanford.edu/
- https://en.wikipedia.org/wiki/Head-driven_phrase_structure_grammar

# License

Copyright © 2016 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
