[![Clojars Project](https://img.shields.io/clojars/v/babylon.svg)](https://clojars.org/babylon)
[![Build Status](https://secure.travis-ci.org/ekoontz/babylon.png?branch=master)](http://travis-ci.org/ekoontz/babylon)

# babylon

A Clojure library for generation and parsing of natural language expressions.

# Getting Started

```
[Sat 18/12/29 09:18 PST] <ekoontz@MacBo..:~/babylon> 52186  (git)-[master]-
% lein repl
nREPL server started on port 55895 on host 127.0.0.1 - nrepl://127.0.0.1:55895
REPL-y 0.3.7, nREPL 0.2.12
Clojure 1.9.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_121-b13
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (load "babylon/toy")
nil
user=> (in-ns 'babylon.toy)
#namespace[babylon.toy]
babylon.toy=> (demo)
generation:
===
a cat
a cat sleeps
the cat
the dog sleeps
a dog
the dog sleeps
a cat sleeps
the dog
a cat
the dog
===
the cat sleeps
the cat sleeps
the dog sleeps
the dog sleeps
a dog sleeps
the dog sleeps
a cat sleeps
the dog sleeps
the dog sleeps
the cat sleeps
===
the cat
the dog
a cat
the dog
a cat
the cat
the dog
the dog
a cat
the dog
parsing:
===
[rule-1  .[rule-1  .the *cat] *sleeps]
[rule-1  .[rule-1  .a *dog] *sleeps]
[rule-1  .[rule-1  .the *dog] *sleeps]
[rule-1  .[rule-1  .the *dog] *sleeps]
[rule-1  .[rule-1  .a *cat] *sleeps]
[rule-1  .[rule-1  .a *dog] *sleeps]
[rule-1  .[rule-1  .a *cat] *sleeps]
[rule-1  .[rule-1  .the *cat] *sleeps]
[rule-1  .[rule-1  .the *dog] *sleeps]
[rule-1  .[rule-1  .a *dog] *sleeps]
10
babylon.toy=>
```


# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
