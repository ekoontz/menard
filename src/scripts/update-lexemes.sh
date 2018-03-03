echo "(do (use 'babel.directory)
          (use 'babel.korma)
          (use 'korma.db)
          (init-db)
          (transaction
            (print (str \"it..\"))
            (println
              (babel.italiano.grammar/write-lexicon))

            (print (str \"en..\"))
            (println 
              (babel.english.grammar/write-lexicon))

            (print (str \"es..\"))
            (println
  	      (babel.espanol.grammar/write-lexicon))

            (print (str \"fr..\"))
	    (println (babel.francais.grammar/write-lexicon))))" | lein repl

