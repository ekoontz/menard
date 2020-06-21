#TODO: Adopt the excellent recommendations written here:
# https://tech.davis-hansson.com/p/make/
# (other than the ones already adopted so far).
#TODO: even better, move this all to project.clj, rather than having two
#build files to maintain.
SHELL := bash
.PHONY: all install compile clean
all: install

install: /Users/ekoontz/menard/target/menard-0.0.1-SNAPSHOT.jar

src/menard/english/lexicon/compiled.edn: $(shell find src/menard/english/lexicon -name "*.edn" -not -name compiled.edn)
	echo "(do (load \"menard\")(menard.english/write-compiled-lexicon))" | lein repl

src/menard/nederlands/lexicon/compiled.edn: $(shell find src/menard/nederlands/lexicon -name "*.edn" -not -name compiled.edn)
	echo "(do (load \"menard\")(menard.nederlands/write-compiled-lexicon))" | lein repl

src/menard/english/grammar/compiled.edn: src/menard/english/grammar.edn
	echo "(do (load \"menard\")(menard.english/write-compiled-grammar))" | lein repl

src/menard/nederlands/grammar/compiled.edn: src/menard/nederlands/grammar.edn
	echo "(do (load \"menard\")(menard.nederlands/write-compiled-grammar))" | lein repl

compile: src/menard/english/lexicon/compiled.edn \
         src/menard/nederlands/lexicon/compiled.edn \
         src/menard/english/grammar/compiled.edn \
         src/menard/nederlands/grammar/compiled.edn

/Users/ekoontz/menard/target/menard-0.0.1-SNAPSHOT.jar: compile
	lein install

clean:
	- rm $$(find . -name compiled.edn)

