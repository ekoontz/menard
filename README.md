[![Clojars Project](http://clojars.org/babel/latest-version.svg)](http://clojars.org/babel)
[![Build Status](https://secure.travis-ci.org/ekoontz/babel.png?branch=master)](http://travis-ci.org/ekoontz/babel)

# babel

A Clojure library for generation and parsing of natural language expressions.

# Getting Started

```lein test```

I hope the tests will be useful for demonstrating the library's abilities.

# HPSG (Head Driven Phrase Structure Grammar)

Babel is based on a linguistic theory called HPSG. For more details please see:

(wikipedia)
(hpsg.stanford.edu)
..etc

This is a brief introduction to the key ideas of HPSG.

## Linguistic expressions are DAGs (Directed Acyclic Graphs)

## Heads and complements

All words in a language are either heads or complements. A _head_ word
is a function with a certain number of arguments (one might say it
valence). A _complement_ word, on the other hand takes no arguments
(its valence is 0). An argument of _head_ word might be a complement,
or it might be another _head_.

## Complex lexical representation

The bulk of linguistic knowledge is encoded in lexical entries. A
lexical head, a verb for example, encodes for its arguments.

## Expressions derived via unification.

# Lexicon

## Default rules

# Grammar

# Generation

Generation is a function that takes a specification, a lexicon, and a
grammar and returns an expression.

# Parsing

Parsing is a function that takes an expression, a lexicon, and a
grammar and returns a grammatical representation of the expression.

# Translation

A translation of a source expression into a target expression is a
two-step process: first we determine the semantics of the source
expression, and second, we generate a target expression that has this
identical semantic representation.

## Parsing of the source expression

## Generation of the target expression

# License

Copyright © 2015 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
