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
babylon.toy=> [1]  + done       /Applications/Emacs.app/Contents/MacOS/Emacs-x86_64-10_9


babylon.toy=>

babylon.toy=> Bye for now!
[Sat 18/12/29 09:20 PST] <ekoontz@MacBo..:~/babylon> 52187  (git)-[master]-
%
[Sat 18/12/29 09:20 PST] <ekoontz@MacBo..:~/babylon> 52187  (git)-[master]-
%
[Sat 18/12/29 09:20 PST] <ekoontz@MacBo..:~/babylon> 52187  (git)-[master]-
% realemacs &
[1] 91496
[1]  + 91496 running    /Applications/Emacs.app/Contents/MacOS/Emacs-x86_64-10_9
[Sat 18/12/29 09:20 PST] <ekoontz@MacBo..:~/babylon> 52188  (git)-[master]-
% 2018-12-29 09:20:22.756 Emacs-x86_64-10_9[91496:5145353] Failed to initialize color list unarchiver: Error Domain=NSCocoaErrorDomain Code=4864 "*** -[NSKeyedUnarchiver _initForReadingFromData:error:throwLegacyExceptions:]: non-keyed archive cannot be decoded by NSKeyedUnarchiver" UserInfo={NSDebugDescription=*** -[NSKeyedUnarchiver _initForReadingFromData:error:throwLegacyExceptions:]: non-keyed archive cannot be decoded by NSKeyedUnarchiver}

[1]  + 91496 running    /Applications/Emacs.app/Contents/MacOS/Emacs-x86_64-10_9
[Sat 18/12/29 09:34 PST] <ekoontz@MacBo..:~/babylon> 52188  (git)-[master]-
%
[1]  + 91496 running    /Applications/Emacs.app/Contents/MacOS/Emacs-x86_64-10_9
[Sat 18/12/29 09:34 PST] <ekoontz@MacBo..:~/babylon> 52188  (git)-[master]-
% lein repl
nREPL server started on port 56003 on host 127.0.0.1 - nrepl://127.0.0.1:56003
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
the cat sleeps
the cat sleeps
a dog
the cat
the dog
the cat sleeps
a dog
a cat sleeps
the dog
===
a dog sleeps
the cat sleeps
a cat sleeps
a dog sleeps
a dog sleeps
a dog sleeps
the cat sleeps
the dog sleeps
the cat sleeps
a dog sleeps
===
the dog
the cat
a cat
the dog
a dog
the cat
the dog
a cat
a cat
a dog
parsing:
===
[rule-1  .a *cat]
[rule-1  .[rule-1  .the *dog] *sleeps]
[rule-1  .the *dog]
[rule-1  .the *dog]
[rule-1  .a *cat]
[rule-1  .[rule-1  .the *dog] *sleeps]
[rule-1  .the *dog]
[rule-1  .the *cat]
[rule-1  .[rule-1  .the *cat] *sleeps]
[rule-1  .the *cat]
10
babylon.toy=> [1]  + done       /Applications/Emacs.app/Contents/MacOS/Emacs-x86_64-10_9


babylon.toy=>

babylon.toy=> (load "babylon/english")

FileNotFoundException Could not locate babylon/babylon/english__init.class or babylon/babylon/english.clj on classpath.  clojure.lang.RT.load (RT.java:463)
babylon.toy=> Bye for now!
[Sun 18/12/30 09:45 PST] <ekoontz@MacBo..:~/babylon> 52189  (git)-[more-english-morphology]-
% lein repl
nREPL server started on port 58498 on host 127.0.0.1 - nrepl://127.0.0.1:58498

REPL-y 0.3.7, nREPL 0.2.12
Clojure 1.9.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_121-b13
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=>

user=> (load "babylon/english")
nil
user=> (in-ns 'babylon.english)
#namespace[babylon.english]
babylon.english=> (demo)
Generation:
===

IllegalStateException Attempting to call unbound fn: #'babylon.grammar/default-morph-fn  clojure.lang.Var$Unbound.throwArity (Var.java:45)
babylon.english=> (load "babylon/grammar")

FileNotFoundException Could not locate babylon/babylon/grammar__init.class or babylon/babylon/grammar.clj on classpath.  clojure.lang.RT.load (RT.java:463)
babylon.english=> (load "grammar")
nil
babylon.english=> (demo)
Generation:
===
a cat sleeps
the cat sleeps
the cat
a dog sleeps
the cat sleeps
a dog
the dog
the cat sleeps
the dog
a cat
===
a cat sleeps
the dog sleeps
a dog sleeps
the dog sleeps
the cat sleeps
the dog sleeps
the cat sleeps
the cat sleeps
the dog sleeps
a dog sleeps
===
the cat
a dog
the cat
a cat
a cat
a dog
the cat
the dog
a cat
the dog
Parsing:
===
[rule-1  .a *dog]
[rule-1  .[rule-1  .the *cat] *sleeps]
[rule-1  .a *dog]
[rule-1  .the *cat]
[rule-1  .a *cat]
[rule-1  .[rule-1  .a *cat] *sleeps]
[rule-1  .[rule-1  .a *cat] *sleeps]
[rule-1  .[rule-1  .a *cat] *sleeps]
[rule-1  .the *dog]
[rule-1  .a *cat]
10
babylon.english=> Bye for now!
[Sun 18/12/30 09:47 PST] <ekoontz@MacBo..:~/babylon> 52190  (git)-[more-english-morphology]-
% lein repl
nREPL server started on port 58506 on host 127.0.0.1 - nrepl://127.0.0.1:58506
REPL-y 0.3.7, nREPL 0.2.12
Clojure 1.9.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_121-b13
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (load "babylon/grammar")
nil
user=> (load "babylon/english")
nil
user=> (in-ns 'babylon.english)
#namespace[babylon.english]
babylon.english=> (demo)
Generation:
===

IllegalStateException Attempting to call unbound fn: #'babylon.grammar/default-morph-fn  clojure.lang.Var$Unbound.throwArity (Var.java:45)
babylon.english=> (load "grammar")
nil
babylon.english=> (demo)
Generation:
===
a dog
the cat sleeps
the dog
the dog
the dog
the dog
the cat
a cat
the dog
the cat
===
a dog sleeps
the cat sleeps
a cat sleeps
the dog sleeps
a dog sleeps
a dog sleeps
a dog sleeps
a cat sleeps
a dog sleeps
a dog sleeps
===
the cat
the dog
a cat
a dog
the dog
a dog
the dog
the cat
the dog
the cat
Parsing:
===
[rule-1  .[rule-1  .a *cat] *sleeps]
[rule-1  .a *cat]
[rule-1  .[rule-1  .the *cat] *sleeps]
[rule-1  .a *dog]
[rule-1  .the *dog]
[rule-1  .[rule-1  .the *cat] *sleeps]
[rule-1  .[rule-1  .the *dog] *sleeps]
[rule-1  .a *dog]
[rule-1  .a *dog]
[rule-1  .[rule-1  .a *cat] *sleeps]
10
babylon.english=> Bye for now!
[Sun 18/12/30 09:49 PST] <ekoontz@MacBo..:~/babylon> 52191  (git)-[more-english-morphology]-
% lein repl
nREPL server started on port 58510 on host 127.0.0.1 - nrepl://127.0.0.1:58510
REPL-y 0.3.7, nREPL 0.2.12
Clojure 1.9.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_121-b13
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (load "babylon/english")
nil
user=> (in-ns 'babylon.english)
#namespace[babylon.english]
babylon.english=> (load "grammar")
nil
babylon.english=> (demo)
Generation:
===
the cat
a cat sleeps
the dog
a dog
a dog sleeps
the dog
a cat
a cat
the dog sleeps
a cat sleeps
===
a cat sleeps
a cat sleeps
a dog sleeps
the cat sleeps
a cat sleeps
the cat sleeps
a dog sleeps
the cat sleeps
the dog sleeps
a dog sleeps
===
a cat
a cat
the cat
a dog
a cat
the cat
a dog
the cat
the cat
the cat
Parsing:
===
[rule-1  .[rule-1  .a *dog] *sleeps]
[rule-1  .[rule-1  .the *cat] *sleeps]
[rule-1  .a *dog]
[rule-1  .the *dog]
[rule-1  .a *cat]
[rule-1  .[rule-1  .a *dog] *sleeps]
[rule-1  .a *cat]
[rule-1  .a *cat]
[rule-1  .a *dog]
[rule-1  .a *cat]
10
babylon.english=> (load "grammar")
nil
babylon.english=> Bye for now!
[Sun 18/12/30 23:55 PST] <ekoontz@MacBo..:~/babylon> 52192  (git)-[more-english-morphology]-
% lein repl
nREPL server started on port 59646 on host 127.0.0.1 - nrepl://127.0.0.1:59646



REPL-y 0.3.7, nREPL 0.2.12
Clojure 1.9.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_121-b13
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=>

user=>

user=>

user=> (load "babylon/english")

CompilerException java.lang.RuntimeException: No such var: m/morph-leaf, compiling:(babylon/grammar.cljc:8:1)
user=> (load "babylon/english")

CompilerException java.lang.Exception: namespace 'babylon.grammar' not found, compiling:(babylon/english.cljc:1:1)
user=> (load "babylon/grammar")

CompilerException java.lang.RuntimeException: No such var: m/morph-leaf, compiling:(babylon/grammar.cljc:8:1)
user=> (load "babylon/morphology")
nil
user=> (load "babylon/grammar")

CompilerException java.lang.RuntimeException: No such var: m/morph-leaf, compiling:(babylon/grammar.cljc:8:1)
user=> Bye for now!
f%                                                                                                                                           [Mon 18/12/31 00:05 PST] <ekoontz@MacBo..:~/babylon> 52193  (git)-[more-english-morphology]-
% find . -name morphology.clj
./src/babylon/morphology.clj
[Mon 18/12/31 00:05 PST] <ekoontz@MacBo..:~/babylon> 52194  (git)-[more-english-morphology]-
% find . -name morphology.cljc
./src/babylon/morphology.cljc
[Mon 18/12/31 00:06 PST] <ekoontz@MacBo..:~/babylon> 52195  (git)-[more-english-morphology]-
% git rm src/babylon/morphology.clj
rm 'src/babylon/morphology.clj'
[Mon 18/12/31 00:06 PST] <ekoontz@MacBo..:~/babylon> 52196  (git)-[more-english-morphology]-
% lein repl
nREPL server started on port 59651 on host 127.0.0.1 - nrepl://127.0.0.1:59651
REPL-y 0.3.7, nREPL 0.2.12
Clojure 1.9.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_121-b13
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (load "babylon/english")
nil
user=> (in-ns 'babylon.
babylon.english             babylon.exception           babylon.generate            babylon.generate.truncate   babylon.grammar
babylon.lexiconfn           babylon.morphology          babylon.over                babylon.parse               babylon.ug
user=> (in-ns 'babylon.english)
#namespace[babylon.english]
babylon.english=> (demo)
Generation:
===

IllegalStateException Attempting to call unbound fn: #'babylon.grammar/default-morph-fn  clojure.lang.Var$Unbound.throwArity (Var.java:45)
babylon.english=> (load "grammar")
nil
babylon.english=> (demo)
Generation:
===
the cat
the dog sleeps
a cat sleeps
the cat
a cat sleeps
a cat
the cat sleeps
a cat
a cat
a dog sleeps
===
a cat sleeps
a dog sleeps
a cat sleeps
the dog sleeps
the cat sleeps
a dog sleeps
a cat sleeps
a dog sleeps
the cat sleeps
a dog sleeps
===
the dog
a dog
the dog
the dog
a dog
a dog
the dog
the dog
a dog
a dog
Parsing:
===
[rule-1  .a *dog]
[rule-1  .[rule-1  .the *cat] *sleeps]
[rule-1  .a *dog]
[rule-1  .the *dog]
[rule-1  .the *cat]
[rule-1  .[rule-1  .the *dog] *sleeps]
[rule-1  .the *cat]
[rule-1  .a *dog]
[rule-1  .a *cat]
[rule-1  .the *cat]
10
babylon.english=> Bye for now!
[Mon 18/12/31 00:07 PST] <ekoontz@MacBo..:~/babylon> 52197  (git)-[more-english-morphology]-
% lein repl
nREPL server started on port 59655 on host 127.0.0.1 - nrepl://127.0.0.1:59655
REPL-y 0.3.7, nREPL 0.2.12
Clojure 1.9.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_121-b13
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (in-ns 'babylon.english)
#namespace[babylon.english]
babylon.english=> Bye for now!
[Mon 18/12/31 00:08 PST] <ekoontz@MacBo..:~/babylon> 52198  (git)-[more-english-morphology]-
% lein repl
nREPL server started on port 59659 on host 127.0.0.1 - nrepl://127.0.0.1:59659
REPL-y 0.3.7, nREPL 0.2.12
Clojure 1.9.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_121-b13
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (load "babylon/english")
nil
user=> (in-ns 'babylon.english)
#namespace[babylon.english]
babylon.english=> (demo)
Generation:
===
a cat sleeps
a dog
the dog
the cat sleeps
the dog
the dog
a dog sleeps
the dog
the cat sleeps
the cat sleeps
===
a dog sleeps
the cat sleeps
a dog sleeps
a dog sleeps
a dog sleeps
the dog sleeps
the dog sleeps
the dog sleeps
a dog sleeps
the cat sleeps
===
the dog
a dog
the cat
the dog
a cat
a cat
the cat
the dog
a cat
the dog
Parsing:
===
[rule-1  .the *dog]
[rule-1  .a *cat]
[rule-1  .a *dog]
[rule-1  .the *cat]
[rule-1  .a *dog]
[rule-1  .the *dog]
[rule-1  .the *cat]
[rule-1  .[rule-1  .a *dog] *sleeps]
[rule-1  .the *cat]
[rule-1  .a *dog]
10
babylon.english=> Bye for now!
^[[A%                                                                                                                                        [Mon 18/12/31 22:13 PST] <ekoontz@MacBo..:~/babylon> 52199  (git)-[more-english-morphology]-
% lein repl
nREPL server started on port 61788 on host 127.0.0.1 - nrepl://127.0.0.1:61788
REPL-y 0.3.7, nREPL 0.2.12
Clojure 1.9.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_121-b13
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (load "babylon/english")
nil
user=> (in-ns 'babylon.english)
#namespace[babylon.english]
babylon.english=> (demo)
Generation:
===
a dog
a dog
the dogs
the cats
the cat
the cat
the dogs
the cat
a dog
the dogs
===
a cat sleeps
the dog sleeps
the dogs sleep
the cat sleeps
the dogs sleep
the cat sleeps
the dogs sleep
a dog sleeps
the dogs sleep
the cats sleep
===
the dogs
the cat
the dogs
the dogs
the cat
the dogs
the dogs
a cat
a cat
the dogs
Parsing:
===
[rule-1  .a *cat]
[rule-1  .a *dog]
[rule-1  .the *cat]
[rule-1  .the *dogs]
[rule-1  .a *dog]
[rule-1  .[rule-1  .the *dogs] *sleep]
[rule-1  .a *dog]
[rule-1  .the *cats]
[rule-1  .[rule-1  .a *cat] *sleeps]
[rule-1  .the *dogs]
10
babylon.english=> Bye for now!
[Mon 18/12/31 22:20 PST] <ekoontz@MacBo..:~/babylon> 52200  (git)-[more-english-morphology]-
% lein repl
nREPL server started on port 61820 on host 127.0.0.1 - nrepl://127.0.0.1:61820
REPL-y 0.3.7, nREPL 0.2.12
Clojure 1.9.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_121-b13
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (load "babylon/english")
nil
user=> (in-ns 'babylon.english)
#namespace[babylon.english]
babylon.english=> (demo)
Generation:
===
a dog sleeps
a cat
the dogs
the dogs
a dog
the dogs
a dog sleeps
the dogs
the cats
the dog
===
the dogs sleep
the dog sleeps
the cat sleeps
the cats sleep
the cats sleep
the cat sleeps
a dog sleeps
a cat sleeps
a cat sleeps
a cat sleeps
===
a dog
the dog
the dogs
the dogs
the cats
a cat
the dogs
the dog
a dog
the dogs
Parsing:
===
[rule-1  .[rule-1  .the *cat] *sleeps]
[rule-1  .the *dogs]
[rule-1  .[rule-1  .a *cat] *sleeps]
[rule-1  .a *dog]
[rule-1  .[rule-1  .a *dog] *sleeps]
[rule-1  .a *dog]
[rule-1  .the *dog]
[rule-1  .the *cats]
[rule-1  .[rule-1  .the *cats] *sleep]
[rule-1  .the *dogs]
10
babylon.english=> Bye for now!
[Mon 18/12/31 22:25 PST] <ekoontz@MacBo..:~/babylon> 52201  (git)-[more-english-morphology]-
% lein repl
nREPL server started on port 61880 on host 127.0.0.1 - nrepl://127.0.0.1:61880
REPL-y 0.3.7, nREPL 0.2.12
Clojure 1.9.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_121-b13
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (load "babylon/english")
nil
user=> (in-ns 'babylon.english)
#namespace[babylon.english]
babylon.english=> (demo)
Generation:
===
a dog
the cat sleeps
the cat
the dogs
the cats sleep
the cat
a cat
the cat
the cats
the cat
===
the dogs sleep
the dogs sleep
the cats sleep
a dog sleeps
the cats sleep
the cats sleep
the cats sleep
the cat sleeps
a dog sleeps
the dogs sleep
===
the dogs
a cat
the dogs
the cats
the cat
the cats
the dogs
the dogs
the cat
the cat
Parsing:
===
[rule-1  .a *cat]
[rule-1  .a *cat]
[rule-1  .[rule-1  .the *dogs] *sleep]
[rule-1  .[rule-1  .a *dog] *sleeps]
[rule-1  .the *dogs]
[rule-1  .the *cats]
[rule-1  .[rule-1  .a *cat] *sleeps]
[rule-1  .[rule-1  .the *dogs] *sleep]
[rule-1  .a *cat]
[rule-1  .a *cat]
10
babylon.english=> (load "english")
nil
babylon.english=> (demo)
Generation:
===
the cat sleeps
the cat sleeps
the cat sleeps
the dog sleeps
the cat sleeps
a dog sleeps
a dog sleeps
the dogs sleep
the cat sleeps
the dog sleeps
===
the cats sleep
a cat sleeps
the dogs sleep
the cats sleep
a cat sleeps
a dog sleeps
the dog sleeps
the cat sleeps
the cat sleeps
the cats sleep
===
the cats
the dog
the cats
the dog
the cats
a dog
the cat
the dog
the cats
a dog
===
the cat sees itself
the cats see the cats
the cat sees the cats
a dog sees the cats
the dog sees itself
a dog sees itself
the dog sees a dog
the cat sees the cats
the dog sees the cats
the dogs see themselves
===
a dog sees themselves
the cats see itself
the cat sees itself
the cat sees the cats
the dog sees itself
a cat sees itself
the cat sees the dogs
the cats see themselves
a cat sees itself
the dog sees itself
Parsing:
===
[s  .[np  .the *cat] *sleeps], [s  .[np  .the *cat] *sleeps]
[s  .[np  .a *dog] *sleeps], [s  .[np  .a *dog] *sleeps]
[s  .[np  .the *cat] *sleeps], [s  .[np  .the *cat] *sleeps]
[s  .[np  .a *dog] *sleeps], [s  .[np  .a *dog] *sleeps]
[s  .[np  .a *cat] *sleeps], [s  .[np  .a *cat] *sleeps]
[s  .[np  .the *dogs] *sleep], [s  .[np  .the *dogs] *sleep]
[s  .[np  .a *cat] *sleeps], [s  .[np  .a *cat] *sleeps]
[s  .[np  .a *dog] *sleeps], [s  .[np  .a *dog] *sleeps]
[s  .[np  .a *dog] *sleeps], [s  .[np  .a *dog] *sleeps]
[s  .[np  .the *cat] *sleeps], [s  .[np  .the *cat] *sleeps]
10
babylon.english=> (load "english")

CompilerException java.lang.RuntimeException: No such var: g/generate2, compiling:(babylon/english.cljc:109:7)
babylon.english=> (load "english")
nil
babylon.english=> Bye for now!
[Sat 19/01/12 20:14 PST] <ekoontz@MacBo..:~/babylon> 52202  (git)-[rethink-lexicon]-
% cat README.md
[![Clojars Project](https://img.shields.io/clojars/v/babylon.svg)](https://clojars.org/babylon)
[![Build Status](https://secure.travis-ci.org/ekoontz/babylon.png?branch=master)](http://travis-ci.org/ekoontz/babylon)

# babylon

A Clojure library for generation and parsing of natural language expressions.

# Getting Started

```
[Sat 19/01/12 20:14 PST] <ekoontz@MacBo..:~/babylon> 52203  (git)-[master]-
% lein repl
nREPL server started on port 61540 on host 127.0.0.1 - nrepl://127.0.0.1:61540
REPL-y 0.3.7, nREPL 0.2.12
Clojure 1.9.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_121-b13
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (load "babylon/english")
nil
user=> (in-ns 'babylon.english)
#namespace[babylon.english]
babylon.english=> (demo)
Generation:
===
itself sleeps
themselves sleeps
dogs sleeps
the sleeps
kittens sleeps
itself sleeps
kittens sleeps
dogs see
puppies see
cats sleeps
===
itself sleeps
cats see
cats sleeps
dogs see
kittens see
puppies sleeps
kittens see
puppies see
the sleeps
kittens see
===
a cat
a dog
the dogs
a puppy
the puppy
the cats
a kitten
the kitten
the cat
the dogs
===
the dog sees itself
the cat sees itself
the puppy sees itself
cats see themselves
a puppy sees itself
a dog sees itself
the puppy sees itself
the cat sees itself
puppies see themselves
a kitten sees itself
===
kittens see
puppies see
dogs see
cats see
dogs see
kittens see
puppies see
puppies see
dogs see
cats see
Parsing:
===
[s  .kittens *see]
[s  .kittens *sleeps]
[s  .a *sleeps]
[s  .kittens *see]
[s  .itself *sleeps]
[s  .cats *see]
[s  .puppies *see]
[s  .a *sleeps]
[s  .cats *see]
[s  .cats *sleeps]
10
babylon.english=>
```


# License

Copyright © 2018 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.
