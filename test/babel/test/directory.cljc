(ns babel.test.directory
  (:require [babel.test.test :as btest]
            [babel.english.grammar :as english]
            [babel.espanol.grammar :as espanol]
            [babel.francais.grammar :as francais]
            [babel.italiano.lexicon :as italiano]
            [babel.lexiconfn :refer [write-lexicon]]
            [clojure.test :refer [deftest is]]))

(deftest write-lexicons
  (btest/init-db)
  (write-lexicon "en" (english/compile-lexicon))
  (write-lexicon "es" (espanol/compile-lexicon))
  (write-lexicon "fr" (francais/compile-lexicon))
  (write-lexicon "it" (italiano/compile-lexicon)))

