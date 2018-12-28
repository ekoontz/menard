[![Clojars Project](https://img.shields.io/clojars/v/babylon.svg)](https://clojars.org/babylon)
[![Build Status](https://secure.travis-ci.org/ekoontz/babylon.png?branch=master)](http://travis-ci.org/ekoontz/babylon)

# babylon

A Clojure library for generation and parsing of natural language expressions.

# Getting Started

```
% lein repl
nREPL server started on port 55243 on host 127.0.0.1 - nrepl://127.0.0.1:55243
REPL-y 0.3.7, nREPL 0.2.12
Clojure 1.9.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_121-b13
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (load "babylon/generate/toy")
nil
user=> (in-ns 'babylon.generate.toy)
#namespace[babylon.generate.toy]
babylon.generate.toy=> (demo)
the dog sleeps
the dog sleeps
the cat sleeps
a cat sleeps
the dog
the dog
the cat sleeps
the dog sleeps
a dog
a cat
===
the dog sleeps
a cat sleeps
a cat sleeps
a cat sleeps
the cat sleeps
the cat sleeps
a dog sleeps
the dog sleeps
the dog sleeps
a cat sleeps
===
a dog
the cat
a dog
the cat
the cat
the cat
a dog
the cat
the dog
a cat
10
babylon.generate.toy=> 
```


# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
