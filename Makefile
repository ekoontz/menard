.PHONY: all install compile clean

all: install

install: /Users/ekoontz/babylon/target/babylon-0.0.1-SNAPSHOT.jar

src/babylon/english/lexicon/compiled.edn:
	echo "(do (load \"babylon\")(babylon.english/write-compiled-lexicon))" | lein repl

src/babylon/nederlands/lexicon/compiled.edn:
	echo "(do (load \"babylon\")(babylon.nederlands/write-compiled-lexicon))" | lein repl

compile: src/babylon/english/lexicon/compiled.edn src/babylon/nederlands/lexicon/compiled.edn

/Users/ekoontz/babylon/target/babylon-0.0.1-SNAPSHOT.jar: compile
	lein install

clean:
	- rm src/babylon/english/lexicon/compiled.edn src/babylon/nederlands/lexicon/compiled.edn
