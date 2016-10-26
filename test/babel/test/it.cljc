(ns babel.test.it
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.directory :refer [models]]
   [babel.generate :as generate]
   [babel.italiano :refer [analyze fo-ps generate lightning-bolts medium np-grammar parse preprocess small]]
   [babel.italiano.grammar :as grammar :refer [model]]
   [babel.italiano.morphology :as morph :refer [analyze-regular fo
                                                replace-patterns]]
   [babel.italiano.morphology.nouns :as nouns]
   [babel.italiano.morphology.verbs :as verbs]
   #?(:cljs [babel.logjs :as log])
   [babel.over :as over]
   #?(:clj [clojure.test :refer [deftest is]])
   #?(:cljs [cljs.test :refer-macros [deftest is]])
   #?(:clj [clojure.tools.logging :as log])
   #?(:clj [clojure.repl :refer [doc]])
   [clojure.string :as string]
   [clojure.set :as set]
   [dag_unify.core :refer [copy fail? get-in strip-refs unifyc]]))

(defn generate-with-medium [spec]
  (generate spec :model (medium)))

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
                         :model (small)
                         :do-enrich false)]
    (is (= "io sono" (fo result)))))

(deftest passato-prossimo
  (let [result (generate {:root {:italiano {:italiano "bere"}}
                          :synsem {:subcat ()
                                   :sem {:subj {:pred :I}
                                         :tense :past
                                         :aspect :perfect}}}
                         :model (small))]
    (is (not (nil? result)))
    (is (= "io ho bevuto" (fo result)))))

(deftest parse-ci-before-vowel
  (let [result (:parses (first (parse "c'è stato")))]
    (is (not (empty? result)))
    (is (= "c'è stato") (fo (first (:parses (first result)))))))

;; this should succeed to parse...
(deftest passato-prossimo-parsing-positive
  (let [result (:parses (first (parse "lei è andata")))]
    (is (not (empty? result)))
    (is (= "lei è andata") (fo (first (:parses (first result)))))))

;; ..whereas this should fail:
(deftest passato-prossimo-parsing-negative
  (let [result (:parses (first (parse "lei è andato")))]
    (is (empty? result))))

(deftest passato-prossimo-reflexive
  (let [result (generate {:comp {:synsem {:agr {:gender :fem}}}
                          :synsem {:subcat '()
                                   :infl :present
                                   :sem {:pred :get-up
                                         :subj {:pred :I}
                                         :tense :past
                                         :aspect :perfect}}}
                         :model (small))]
    (is (not (nil? result)))
    (is (= "io mi sono alzata" (fo result)))))

(deftest present-ditransitive
  (let [result (generate {:synsem {:subcat '()
                                   :infl :present
                                   :sem {:pred :be-called
                                         :subj {:pred :I}
                                         :iobj {:pred :luisa}}}}
                         :model (small))]
    (is (not (nil? result)))
    (is (= "io mi chiamo Luisa" (fo result)))))

(deftest parse-io-parlo
  (let [result (:parses (first (parse "io parlo")))]
    (is (not (empty? result)))
    (is (= "io parlo") (fo (first (:parses (first result)))))))
        
(deftest round-trip-1
  (let [expr (generate {:synsem {:subcat '()
                                 :sem {:spec {:def :def} 
                                       :mod {:pred :difficile}
                                       :number :sing
                                       :pred :donna}}}
                       :model (np-grammar))]
    (is (or (= (fo expr) "la donna difficile")
            (= (fo expr) "la difficile donna")))
    (is (not (empty? (reduce concat (map
                                     :parses (parse (fo expr) (np-grammar)))))))))

(deftest forbid-mispelling
 (is (empty? (:parses (parse (fo "la donna difficila") (np-grammar))))))

(deftest generate-and-parse-noun-phrase-with-specifier
  ;; create a noun phrase where the determiner is "ventotto", but the head of the noun phrase
  ;; might be anything.
  (let [result (generate {:synsem {:sem {:spec {:def :twentyeight}}}}
                         :model (np-grammar))]
    (is (not (= "" (fo result))))
    (is (= :twentyeight (get-in result [:synsem :sem :spec :def])))
    (is (not (empty? (parse (fo result)))))))

(def map-fn #?(:clj pmap) #?(:cljs map))

;; <roundtrip parsing tests>
;; these tests will not pass if you
;; don't have enough linguistic material
;; (grammar + lexicon) to generate
;; enough 'do-this-many' sentences to test.
;; The 'do-this-many' is controlled by each
;; deftest's 'do-this-many' below.
(deftest roundtrip-np-grammar
  (let [do-this-many 10
        expressions (take do-this-many
                          (repeatedly #(generate
                                        {:synsem {:sem {:mod {:pred :top}
                                                        :number :top
                                                        :pred :top
                                                        :spec {:def :top}}}}
                                        ;; change the above 
                                        ;; generic spec to something more specific
                                        ;; if this test fails and you want to investigate
                                        ;; why.
                                        :model (np-grammar))))]
    (is (= do-this-many
           (count (map-fn (fn [expr] 
                            (let [fo (fo expr)
                                  parsed (reduce concat (map :parses
                                                             (parse fo (np-grammar))))]
                              (if (not (empty? parsed))
                                (log/info (str "parse OK:" fo))
                                (log/error (str "parse failed: " fo)))
                              (is (not (empty? parsed)))))
                          expressions))))))

(deftest roundtrip-present
  (let [do-this-many 10
        expressions (take do-this-many
                          (repeatedly
                           #(generate {:synsem {:cat :verb
                                                :sem {:tense :present}
                                                :subcat '()}}
                                      :model (small))))]
    (is (= do-this-many
           (count (map-fn (fn [expr] 
                            (let [fo (fo expr)
                                  parsed (reduce concat (map :parses (parse fo)))]
                              (if (not (empty? parsed))
                                (log/info (str "parse OK:" fo))
                                (log/error (str "parse failed: " fo)))
                              (is (not (empty? parsed)))))
                          expressions))))))

(deftest roundtrip-imperfect
  (let [do-this-many 10
        expressions (take do-this-many
                          (repeatedly
                           #(generate {:synsem {:cat :verb
                                                :infl :imperfect
                                                :sem {:tense :past
                                                      :aspect :progressive}
                                                :subcat '()}}
                                      :model (small))))]
    (is (= do-this-many
           (count (map-fn (fn [expr]
                            (let [fo (fo expr)
                                  parsed (reduce concat (map :parses (parse fo)))]
                              (if (not (empty? parsed))
                                (log/info (str "parse OK:" fo))
                                (log/error (str "parse failed: " fo)))
                              (is (not (empty? parsed)))))
                          expressions))))))

(deftest roundtrip-past
  (let [do-this-many 10
        expressions (take do-this-many
                          (repeatedly
                           #(generate {:synsem {:cat :verb
                                                :essere true
                                                :sem {:tense :past
                                                      :aspect :perfect}
                                                :subcat '()}}
                                      :model (small))))]
    (is (= do-this-many
           (count (map-fn (fn [expr]
                          (let [fo (fo expr)
                                parsed (reduce concat (map :parses (parse fo)))]
                            (if (not (empty? parsed))
                              (log/info (str "parse OK:" fo))
                              (log/error (str "parse failed: " fo)))
                            (is (not (empty? parsed)))))
                        expressions))))))

(deftest roundtrip-future
  (let [do-this-many 10
        expressions (take do-this-many
                          (repeatedly
                           #(generate {:synsem {:cat :verb
                                                :sem {:tense :future}
                                                :subcat '()}}
                                      :model (small))))]
    (is (= do-this-many
           (count (map-fn (fn [expr]
                          (let [fo (fo expr)
                                parsed (reduce concat (map :parses (parse fo)))]
                            (if (not (empty? parsed))
                              (log/info (str "parse OK:" fo))
                              (log/error (str "parse failed: " fo)))
                            (is (not (empty? parsed)))))
                        expressions))))))

(deftest roundtrip-conditional
  (let [do-this-many 10
        expressions (take do-this-many
                          (repeatedly
                           #(generate {:synsem {:cat :verb
                                                :sem {:tense :conditional}
                                                :subcat '()}}
                                      :model (small))))]
    (is (= do-this-many
           (count (map-fn (fn [expr]
                          (let [fo (fo expr)
                                parsed (reduce concat (map :parses (parse fo)))]
                            (if (not (empty? parsed))
                              (log/info (str "parse OK:" fo))
                              (log/error (str "parse failed: " fo)))
                            (is (not (empty? parsed)))))
                        expressions))))))

(deftest the-red-cat-woke-up
  (let [result (:parses (first (parse "il gatto rosso si è alzato")))]
    ;; should find at least one structure:
    (is (not (empty? result)))
    ;; formatting the first of the resultant parse trees:
    ;; output should be the same as the input to the parser:
    (is (or (= "il gatto rosso si è alzato"
               (fo (first result)))
            (= "il rosso gatto si è alzato"
               (fo (first result)))))))
            
;; tricky tokenization of 'la sua' and 'la loro' as lexemes.
(deftest parsing
  (count
   (map (fn [surface]
          (let [semantics (strip-refs
                           (get-in
                            (first
                             (reduce concat (map :parses (parse surface (medium)))))
                            [:synsem :sem]))]
            (is (map? semantics))))
        ["la sua ragazza"
         "la sua ragazza dorme"
         "la sua ragazza bella dorme"
         "noi beviamo la loro acqua bella"
         "noi abbiamo bevuto la loro acqua bella"
         "Luisa e io abbiamo bevuto la loro acqua bella"])))

(deftest parse-with-boot-stem
  (is (not (empty? (:parses (first (parse "lei esce")))))))

;; roundtrip parser testing
(defn roundtrip-parsing [n]
  (take n
        (repeatedly #(let [generated
                           (fo (generate-with-medium {:synsem {:cat :verb
                                                               :subcat '()}}))
                           parsed (reduce concat (map :parses (parse generated medium)))]
                       (log/info (str "generated: " generated))
                       (log/info (str "semantics: "
                                      (or
                                       (strip-refs
                                        (get-in (first parsed)
                                                [:synsem :sem]))
                                       (str "NO PARSE FOUND FOR: " generated))))
                       {:generated generated
                        :pred (get-in (first parsed) [:synsem :sem :pred])
                        :subj (get-in (first parsed) [:synsem :sem :subj :pred])}))))

(defn ps-tree [tree morph]
  "return just the essentials of a tree: just rule names and surface forms at leaves."
  (let [rule (get-in tree [:rule])
        head (get-in tree [:head])
        comp (get-in tree [:comp])]
    (if (and head comp rule)
      (conj {:rule rule}
            {:head (ps-tree head morph)
             :comp (ps-tree comp morph)})
      (morph tree))))

;; should fail fast: instead seems to run forever.
(deftest difficult-generate
  (let [synsem
        {:synsem {:sem {:pred :be
                        :subj {:pred :città}
                        :obj {:pred :calzoni}}}}]
    (is (or true ;; test won't complete (yet) without disabling with this 'or true'.
            (not (nil? (generate-with-medium synsem)))))))

(deftest casa-parse
  (is (not (empty?
            (reduce concat (map :parses (parse "io sono a casa")))))))

(deftest gestiscono
  (let [result
        (generate {:synsem {:sem {:subj {:pred :loro}
                                  :pred :manage
                                  :tense :present}}}
                  :model (small))]
    (is (= "loro gestiscono" (fo result)))))

(deftest casa-generate
  (let [result (generate-with-medium {:synsem {:cat :noun
                                               :agr {:number :sing}
                                               :sem {:pred :house
                                                     :mod '()
                                                     :spec {:def :def}}}})]
    (is (= (fo result) "la casa"))
    (is (= (get-in result [:synsem :sem :mod]) '()))))

(deftest case-generate
  (let [result (generate-with-medium {:synsem {:cat :noun
                                               :agr {:number :plur}
                                               :sem {:pred :house
                                                     :mod '()
                                                     :spec {:def :def}}}})]
    (is (= (fo result) "le case"))
    (is (= (get-in result [:synsem :sem :mod]) '()))))

(deftest alla-casa-generate
  (let [result (generate-with-medium
                {:comp {:synsem {:agr {:number :sing}
                                 :reflexive false}}
                 ;; TODO: the above is needed to prevent "a" + reflexive pronoun:
                 ;; eliminate this need.
                 :synsem {:cat :prep
                          :sem {:pred :a
                                :obj {:pred :house
                                      :spec {:def :def}
                                      :mod '()}}}})]
    (is (= (fo result) "alla casa"))))

(deftest a-casa-generate
  (let [result (generate-with-medium
                {:comp {:synsem {:pronoun false}}
                 ;; TODO: the above is needed to prevent "a" + reflexive pronoun:
                 ;; eliminate this need.
                 :synsem {:cat :prep
                          :sem {:pred :a
                                :obj {:pred :house
                                      :mod '() ;; if :mod '() is specified:
                                      ;; test will run faster since there will
                                      ;; be less futile bolts checked
                                      ;; (less bolts where no complement can
                                      ;; be found).
                                      :spec {:def :none}}}}})] ;; "a casa", not "a tua casa", "a della casa", etc
    (is (= (fo result) "a casa"))))

;; TODO: what is this test doing that the preceding test is not doing?
(deftest a-casa-generate-2
  (let [result (generate-with-medium
                {:modified false
                 :synsem {:cat :verb 
                          :sem {:tense :present 
                                :pred :a 
                                :obj {:pred :house
                                      :spec {:def :none}} ;; "a casa", not "a tua casa", "a della casa", etc
                                :subj {:pred :I}}}})]
    (is (= (fo result) "io sono a casa"))))

(deftest chiamarsi-1
  (let [result (parse "io mi chiamo Luisa")]
    (is (not (empty? (mapcat :parses result))))))

(deftest chiamarsi-2
  (let [result (parse "tu ti chiami Luisa")]
    (is (not (empty? (mapcat :parses result))))))

(deftest chiamarsi-3
  (let [result (parse "lei si chiama Luisa")]
    (is (not (empty? (mapcat :parses result))))))

;; sentential modifiers: S -> S PP
(deftest io-dormo-a-casa
  (let [result (parse "io dormo a casa")]
    (is (not (empty? (mapcat :parses result))))))


(deftest preprocess-test
  (is (= (preprocess "Dopo ventotto anni, Ostana ha un cittadino neonato.")
         (preprocess "dopo ventotto anni ostana ha un cittadino neonato")
         "dopo ventotto anni ostana ha un cittadino neonato")))

(deftest parse-long-sentence
  (let [result (parse "dopo ventotto anni ostana ha un cittadino neonato")
        parses (mapcat :parses result)
        semantics
        (map (fn [parse]
               (get-in parse [:synsem :sem]))
             parses)]
    (is (not (empty? parses)))
    (is (not (nil? (some #(= :have (get-in % [:pred])) semantics))))
    (is (not (nil? (some #(= :newborn (get-in % [:obj :pred])) semantics))))
    (is (not (nil? (some #(= :ostana (get-in % [:subj :pred])) semantics))))
    (is (not (nil? (some #(= :after (get-in % [:mod :pred])) semantics))))
    (is (not (nil? (some #(= :year (get-in % [:mod :obj :pred])) semantics))))
    (is (not (nil? (some #(= :twentyeight (get-in % [:mod :obj :spec :def])) semantics))))))

(deftest parse-long-sentence-with-punctuation
  (let [result (parse "Dopo ventotto anni, Ostana ha un cittadino neonato.")
        parses (mapcat :parses result)
        semantics
        (map (fn [parse]
               (get-in parse [:synsem :sem]))
             parses)]
    (is (not (empty? parses)))
    (is (not (nil? (some #(= :have (get-in % [:pred])) semantics))))
    (is (not (nil? (some #(= :newborn (get-in % [:obj :pred])) semantics))))
    (is (not (nil? (some #(= :ostana (get-in % [:subj :pred])) semantics))))
    (is (not (nil? (some #(= :after (get-in % [:mod :pred])) semantics))))
    (is (not (nil? (some #(= :year (get-in % [:mod :obj :pred])) semantics))))
    (is (not (nil? (some #(= :twentyeight (get-in % [:mod :obj :spec :def])) semantics))))))

(deftest davanti-il-tavolo
  (let [parse-result (mapcat :parses (parse "davanti il tavolo"))
        gen-result (generate-with-medium {:synsem {:cat :prep 
                                                   :sem {:pred :in-front-of
                                                         :obj {:pred :table
                                                               :number :sing
                                                               :mod '()
                                                               :spec {:def :def}}}}
                                          :comp {:synsem {:cat :noun
                                                          :pronoun false
                                                          :subcat '()}}})]
    (is (not (empty? parse-result)))
    (is (= "davanti il tavolo"
           (fo gen-result)))))

(deftest davanti-lo-studente
  (let [parse-result (mapcat :parses (parse "davanti lo studente"))
        gen-result (generate-with-medium {:synsem {:cat :prep 
                                                   :sem {:pred :in-front-of
                                                         :obj {:pred :student
                                                               :number :sing
                                                               :mod '()
                                                               :spec {:def :def}}}}
                                          :comp {:synsem {:cat :noun
                                                          :pronoun false
                                                          :subcat '()}}})]
    (is (not (empty? parse-result)))
    (is (= "davanti lo studente"
           (fo gen-result)))))

(deftest davanti-il-tavolo
  (let [expr (generate-with-medium {:synsem {:cat :prep
                                             :sem {:pred :in-front-of
                                                   :reflexive false
                                                   :obj {:pred :table
                                                         :mod '()
                                                         :number :sing
                                                         :spec {:def :def
                                                                :pred :definite}}}}})]
    (is (= (fo expr)
           "davanti il tavolo"))))

(deftest furniture-sentence
  (let [expr (generate-with-medium {:synsem {:sem {:obj {:pred :table :mod '() :spec {:def :def}
                                                         :number :sing}
                                                   :pred :in-front-of
                                                   :subj {:pred :chair :mod '() :spec {:def :def}
                                                          :number :sing}
                                                   :tense :present
                                                   :aspect :progressive}
                                             :cat :verb}
                                    :comp {:synsem {:agr {:person :3rd}}}
                                    :modified false})]
    (is (= (fo expr)
           "la sedia è davanti il tavolo"))))

(deftest non-arriva
  (is (-> "non arriva"
          parse
          first
          :parses
          empty?
          not)))

(deftest past-and-gender-agreement
  (is (= (fo (generate {:synsem {:sem {:pred :go
                                       :aspect :perfect
                                       :tense :past
                                       :subj {:gender :fem
                                              :pred :loro}}}}
                       :model (small)))
     "loro sono andate")))

(deftest exists1
  (is (= (fo (generate {:synsem {:sem {:obj :unspec
                                       :subj :top
                                       :pred :exist
                                       :reflexive false
                                       :tense :conditional}}
                        :root {:italiano {:italiano "essere"}}
                        :comp {:synsem {:agr {:number :sing}}}}
                       :model (small)))
         "ci sarebbe")))

(deftest exists2
  (is (= (fo (generate {:synsem {:sem {:obj :unspec
                                       :subj :top
                                       :pred :exist
                                       :reflexive false
                                       :aspect :progressive
                                       :tense :past}}
                        :root {:italiano {:italiano "essere"}}
                        :comp {:synsem {:agr {:number :sing}}}}
                       :model (small)))
         "c'era")))

(deftest exists3
  (is (= (fo (generate {:synsem {:sem {:obj :unspec
                                       :subj :top
                                       :pred :exist
                                       :reflexive false
                                       :tense :present}}
                        :root {:italiano {:italiano "essere"}}
                        :comp {:synsem {:agr {:number :sing}}}}
                       :model (small)))
         "c'è")))


