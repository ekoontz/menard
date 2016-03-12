(ns babel.test.it
  (:refer-clojure :exclude [get-in])
  (:require [babel.italiano.grammar :refer [small medium np-grammar]]
            [babel.italiano.lexicon :refer [lexicon]]
            [babel.italiano.morphology :as morph :refer [analyze-regular fo replace-patterns]]
            [babel.italiano.morphology.nouns :as nouns]
            [babel.italiano.morphology.verbs :as verbs]
            [babel.italiano.workbook :refer [analyze generate generate-all parse]]
            [babel.parse :as parse]
            #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log])
            [clojure.string :as string]
            [dag_unify.core :refer [get-in strip-refs]]))

(deftest analyze-1
  (let [singular (analyze "compito")
        plural  (analyze "compiti")]
    (is (not (empty? singular)))
    (is (not (empty? plural)))))

(deftest analyze-2
  (let [singular (analyze "difficile")
        plural  (analyze "difficili")]
    (is (not (empty? singular)))
    (is (not (empty? plural)))))

(deftest analyze-3
  (is (not (empty? (analyze "svegliata")))))

(deftest present-irregular
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:pred :be
                                         :subj {:pred :I}
                                         :tense :present}}}
                         small)]
    (is (= "io sono" (fo result)))))

(deftest passato-prossimo
  (let [result (generate {:root {:italiano {:italiano "bere"}}
                          :synsem {:subcat ()
                                   :sem {:subj {:pred :I}
                                         :tense :past
                                         :aspect :perfect}}}
                         small)]
    (is (not (nil? result)))
    (is (= "io ho bevuto" (fo result)))))

(deftest passato-prossimo-reflexive
  (let [result (generate {:head {:synsem {:agr {:gender :fem}}}
                          :synsem {:subcat '()
                                   :infl :present
                                   :sem {:pred :get-up
                                         :subj {:pred :I}
                                         :tense :past
                                         :aspect :perfect}}}
                         small)]
    (is (not (nil? result)))
    (is (= "io mi sono alzata" (fo result)))))

(deftest parse-io-parlo
  (let [result (parse "io parlo")]
    (is (not (empty? result)))
    (is (= "io parlo") (fo (first result)))))
        
(deftest round-trip-1
  (let [expr (generate {:synsem {:subcat '()
                                 :sem {:spec {:def :def} 
                                       :mod {:pred :difficile}
                                       :number :sing
                                       :pred :donna}}} 
                       np-grammar)]
    (is (= (fo expr) "la donna difficile"))
    (is (not (empty? (parse (fo expr) np-grammar))))))

(deftest forbid-mispelling
 (is (empty? (parse (fo "la donna difficila") np-grammar))))

(deftest roundtrip-np-grammar
  (let [do-this-many 20
        expressions (take do-this-many
                           (generate-all {:synsem {:sem {:spec {:def :top}
                                                         :mod {:pred :top}
                                                         :number :top
                                                         :pred :top}}}
                                         np-grammar))]
    (is (= 20
           (count (pmap (fn [expr] 
                          (let [fo (fo expr)
                                parsed (parse fo np-grammar)]
                            (if (not (empty? parsed))
                              (log/info (str "parse OK:" fo))
                              (log/error (str "parse failed: " fo)))
                            (is (not (empty? parsed)))))
                        expressions))))))

(deftest roundtrip-present
  (let [do-this-many 20
        expressions (take do-this-many
                          (repeatedly
                           #(generate {:synsem {:cat :verb
                                                :sem {:tense :present}
                                                :subcat '()}}
                                      small)))]
    (is (= 20
           (count (pmap (fn [expr] 
                          (let [fo (fo expr)
                                parsed (parse fo)]
                            (if (not (empty? parsed))
                              (log/info (str "parse OK:" fo))
                              (log/error (str "parse failed: " fo)))
                            (is (not (empty? parsed)))))
                        expressions))))))

(deftest roundtrip-imperfect
  (let [do-this-many 20
        expressions (take do-this-many
                          (repeatedly
                           #(generate {:synsem {:cat :verb
                                                :infl :imperfect
                                                :sem {:tense :past
                                                      :aspect :progressive}
                                                :subcat '()}}
                                      small)))]
    (is (= 20
           (count (pmap (fn [expr]
                          (let [fo (fo expr)
                                parsed (parse fo)]
                            (if (not (empty? parsed))
                              (log/info (str "parse OK:" fo))
                              (log/error (str "parse failed: " fo)))
                            (is (not (empty? parsed)))))
                        expressions))))))

(deftest roundtrip-past
  (let [do-this-many 20
        expressions (take do-this-many
                          (repeatedly
                           #(generate {:synsem {:cat :verb
                                                :essere true
                                                :sem {:tense :past
                                                      :aspect :perfect}
                                                :subcat '()}}
                                      small)))]
    (is (= 20
           (count (pmap (fn [expr]
                          (let [fo (fo expr)
                                parsed (parse fo)]
                            (if (not (empty? parsed))
                              (log/info (str "parse OK:" fo))
                              (log/error (str "parse failed: " fo)))
                            (is (not (empty? parsed)))))
                        expressions))))))

(deftest roundtrip-future
  (let [do-this-many 20
        expressions (take do-this-many
                          (repeatedly
                           #(generate {:synsem {:cat :verb
                                                :sem {:tense :future}
                                                :subcat '()}}
                                      small)))]
    (is (= 20
           (count (pmap (fn [expr]
                          (let [fo (fo expr)
                                parsed (parse fo)]
                            (if (not (empty? parsed))
                              (log/info (str "parse OK:" fo))
                              (log/error (str "parse failed: " fo)))
                            (is (not (empty? parsed)))))
                        expressions))))))

(deftest roundtrip-conditional
  (let [do-this-many 20
        expressions (take do-this-many
                          (repeatedly
                           #(generate {:synsem {:cat :verb
                                                :sem {:tense :conditional}
                                                :subcat '()}}
                                      small)))]
    (is (= 20
           (count (pmap (fn [expr]
                          (let [fo (fo expr)
                                parsed (parse fo)]
                            (if (not (empty? parsed))
                              (log/info (str "parse OK:" fo))
                              (log/error (str "parse failed: " fo)))
                            (is (not (empty? parsed)))))
                        expressions))))))

(deftest the-red-cat-woke-up
  (let [result (parse "il gatto rosso si è alzato")]
    ;; should find at least one structure:
    (is (not (empty? result)))
    ;; formatting the first of the resultant parse trees:
    ;; output should be the same as the input to the parser:
    (is (= "il gatto rosso si è alzato"
           (fo (first result))))))

(defn run-benchmark []
  (repeatedly #(let [debug (println "starting generation")
                     expr (time (generate :top))]
                 (println (str "generated expression: " (fo expr)))
                 (let [parsed (time (first (take 1 (parse (fo expr)))))]
                   (println (str "parsed: " (fo parsed)))
                   (println "")))))




                      
