#TODO: Adopt the excellent recommendations written here:
# https://tech.davis-hansson.com/p/make/
# (other than the ones already adopted so far).
#TODO: even better, move this all to project.clj, rather than having two
#build files to maintain.
SHELL := bash
.PHONY: all install compile clean
all: install

resources/english/lexicon/compiled.edn: $(shell find resources/english/lexicon -name "*.edn" -not -name compiled.edn)
	echo "(do (load \"menard\")(menard.english/write-compiled-lexicon))" | lein repl

resources/nederlands/lexicon/compiled.edn: $(shell find resources/nederlands/lexicon -name "*.edn" -not -name compiled.edn)
	echo "(do (load \"menard\")(menard.nederlands/write-compiled-lexicon))" | lein repl

resources/english/grammar/compiled.edn: resources/english/grammar.edn
	echo "(do (load \"menard\")(menard.english/write-compiled-grammar))" | lein repl

resources/nederlands/grammar/compiled.edn: resources/nederlands/grammar.edn
	echo "(do (load \"menard\")(menard.nederlands/write-compiled-grammar))" | lein repl

compile: resources/english/lexicon/compiled.edn \
         resources/nederlands/lexicon/compiled.edn \
         resources/english/grammar/compiled.edn \
         resources/nederlands/grammar/compiled.edn

clean:
	- rm $$(find . -name compiled.edn)

