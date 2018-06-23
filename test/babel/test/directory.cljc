(ns babel.test.directory
  (:require [babel.test.test :as btest]
            [babel.directory :refer [write-lexicons]]
            [babel.english.grammar :as english]
            [babel.espanol.grammar :as espanol]
            [babel.francais.grammar :as francais]
            [babel.italiano.lexicon :as italiano]
            [babel.lexiconfn :refer [write-lexicon]]
            [clojure.test :refer [deftest is]]))

(deftest write-lexicons-test
  (write-lexicons))

