echo "(do (use 'babel.directory)
    (babel.italiano.grammar/write-lexicon)
    (babel.english.grammar/write-lexicon)
    (babel.espanol.grammar/write-lexicon)
    (babel.francais.grammar/write-lexicon))" | lein repl

