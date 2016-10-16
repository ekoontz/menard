[![Clojars Project](https://img.shields.io/clojars/v/babel.svg)](https://clojars.org/babel)
[![Build Status](https://secure.travis-ci.org/ekoontz/babel.png?branch=master)](http://travis-ci.org/ekoontz/babel)

# babel

A Clojure library for generation and parsing of natural language expressions.

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
