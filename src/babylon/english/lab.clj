(ns babylon.english.lab
  (:require
   [babylon.english :as en :refer [analyze generate grammar index-fn lexicon
                                   morph parse syntax-tree]]
   [babylon.generate :as g :refer [add lazy-map]]
   [dag_unify.core :as u]
   [clojure.tools.logging :as log]))


(def modal-specs
  [{:rule "s"
    :sem {:tense :present}
    :comp {:rule "np"
           :head {:rule "nbar"}}
    :head {:rule "vp-modal-1"
           :comp {:rule "infinitive"
                  :comp {:rule "vp"
                         :comp {:rule "np"
                                :head {:rule "nbar"}}}}}}
   {:rule "s"
    :comp {:phrasal false}
    :head {:phrasal true
           :rule "vp"
           :head {:phrasal false}
           :comp {:phrasal true
                  :rule "np"
                  :head {:phrasal true
                         :rule "nbar3"
                         :comp {:phrasal true
                                :rule "comp1"
                                :head {:phrasal false}
                                :comp {:phrasal false}}}}}}])

(def specs (concat
            modal-specs
            [long-demo-spec]))

(defn demo []
    (repeatedly
      #(println
        (-> (shuffle specs)
            first
            generate
            morph))))

(defn modal-demo []
  (repeatedly
   #(println
     (-> (shuffle modal-specs)
         first
         generate
         time
         morph))))

(defn timed-demo []
  (repeatedly
   #(println
     (-> (shuffle specs)
         first
         generate
         time
         syntax-tree))))

;; for debugging generation and fixing grammatical rules
(defn partial-generate-test []
  (->> [{:rule "s"
         :comp {:phrasal false}
         :head {:rule "vp-modal-2"
                :comp {:phrasal true
                       :modal false
                       :head {:rule "vp"}}}}]
       (lazy-map add)
       (lazy-map add)
       (lazy-map add)
       (lazy-map add)
       (remove #(= :fail %))
       first))

(defn gen
  "how to generate a phrase with particular constraints."
  [i]
  (let [expression (generate (nth specs i))]
      {:st (syntax-tree expression)
       :morph (morph expression)
       :agr (u/strip-refs (u/get-in expression [:head :agr]))
       ;;       :parses (map syntax-tree (parse (morph expression)))
       :phrase? (u/get-in expression [:head :comp :phrasal])}))

(def specific-sentence-spec
  [{:cat :verb
    :subcat []
    :pred :top
    :comp {:phrasal true
           :rule "np"
           :agr {:number :sing
                 :person :3rd}
           :head {:phrasal true
                  :rule "nbar2"
                  :head {:canonical "dog"}}}
    :head {:phrasal false
           :canonical "be"}}])

;; (syntax-tree (generate folding-spec))
(def folding-spec
  {:rule "s"
   :comp {:phrasal false}
   :head {:phrasal true
          :rule "vp-aux"
          :head {:phrasal false}
          :comp {:rule "vp"
                 :phrasal true
                 :head {:phrasal false}
                 :comp {:phrasal false}}}})

;; (syntax-tree (generate nonfolding-spec))
(def nonfolding-spec
  {:rule "s"
   :comp {:rule "np"}
   :head {:rule "vp"
          :head {:canonical "move"
                 :infl :present}
          :comp {:rule "nbar4"
                 :phrasal true
                 :head {:canonical "mother"}
                 :comp {:top :top
                        :phrasal true
                        :rule "comp1"
                        :head {:top :top
                               :canonical "that"}
                        :comp {:top :top
                               :phrasal true
                               :rule "s-slash"
                               :head {:top :top
                                      :rule "vp-aux-slash"}}}}}})
(defn long-s []
  (count
   (take 1
    (repeatedly
      #(println
         (morph
          (generate
           {:rule "s"
            :cat :verb
            :subcat []
            :pred :top
            :comp {:phrasal true
                   :head {:phrasal true}}
            :head {:phrasal true
                   :comp {:phrasal true
                          :head {:phrasal true}}}})))))))

(defn new-test-1 []
  (repeatedly #(println (syntax-tree (generate {:rule "np"
                                                :head {:rule "nbar2"
                                                       :comp {:rule "s-slash" 
                                                              :head {:phrasal true}
                                                              :comp {:phrasal false}}}})))))
(defn new-test-2 []
  (repeatedly #(println (syntax-tree (generate {:rule "np"
                                                :head {:rule "nbar2"
                                                       :comp {:rule "s-slash" 
                                                              :head {:rule "vp-aux-slash"}
                                                              :comp {:phrasal false}}}})))))

(defn matching-lexemes [spec]
  (->> (flatten (vals lexicon))
       (filter #(not (= :fail
                        (u/unify % spec))))))


;; generation reasonable amount of time
(def reasonable-spec
  {:rule "s"
   :head {:phrasal true}
   :comp {:rule "nbar4"
          :head {:phrasal false}
          :comp {:rule "comp1"
                 :comp {:rule "s-slash"
                        :head {:rule "vp-aux-slash"}}}}})

;; generation gets stuck in a dead-end.
(def stuck-spec
  {:rule "s" :variant :present-simple
   :phrasal true :agr {:number :plur}
   :comp {:rule "nbar2"
          :phrasal true
          :head {:phrasal false
                 :canonical :top}
          :comp {:phrasal true
                 :rule "s-slash"
                 :comp {:phrasal false :canonical "cat"}
                 :head {:rule "vp-aux-slash"
                        :phrasal true
                        :head {:phrasal false :canonical "would"}
                        :comp {:phrasal true :canonical :top}}}}
   :head {:phrasal false
          :canonical :top
          :derivation {:2 {:intransitive-verbs true}}}})

;; generation doesn't get stuck, but is unreasonably slow.
(def not-stuck-spec
  {:rule "s" :variant :present-simple
   :phrasal true :agr {:number :plur}
   :comp {:rule "nbar2"
          :phrasal true
          :head {:phrasal false
                 :canonical :top}
          :comp {:phrasal true
                 :rule "s-slash"
                 :comp {:phrasal false :canonical "cat"}
                 :head {:rule "vp-aux-slash"
                        :phrasal true
                        :head {:phrasal false :canonical "would"}}}}
   :head {:phrasal false
          :canonical :top
          :derivation {:2 {:intransitive-verbs true}}}})

;; takes as long as 17 seconds sometimes: 
;; a time books begin would
;; "Elapsed time: 17274.995313 msecs"
(def slow-spec
  {:comp {:rule "np"
          :head {:rule "nbar2"
                 :comp {:rule "s-slash"}}}
   :rule "s"
   :head {:phrasal true
          :comp {:phrasal true}}})


;; (map syntax-tree (parse-as "does she see" "s-interog-slash"))
(defn parse-as
  "return only parses of _expression_ for which the rule is _rule_."
  [expression rule]
  (->> (parse expression)
       (filter #(= rule (u/get-in % [:rule])))))

(def consumer-patience 6000)

(defn timeout-with [timeout-ms callback]
   (let [fut (future (callback))
         ret (deref fut timeout-ms ::timed-out)]
     (when (= ret ::timed-out)
       (println (str "Too bad! you took more than " timeout-ms " msecs long."))
       (future-cancel fut))
     ret))

(defn generate-with-timeout []
    (println (str "Generating.."))
    ((or morph syntax-tree morph) (poetry-line)))

(defn poetry-line-with-timeout []
  (let [result (timeout-with consumer-patience generate-with-timeout)]
    (if (= ::timed-out result)
      (do
        (println (str "retrying.."))
        (poetry-line-with-timeout))
      result)))

(defn poetry-with-timeout []
  (take 50 (repeatedly #(println (time (poetry-line-with-timeout))))))

(def extremely-specific-spec
  {:rule "s"
   :phrasal true
   :head {:rule "vp"
          :phrasal true
          :head {:phrasal false
                 :subcat {:1 {:cat :noun}
                          :2 {:cat :noun}
                          :3 []}}
          :comp {:phrasal true
                 :head {:phrasal true
                        :head {:phrasal false
                               :agr {:number :sing}}
                        :comp {:phrasal false}}
                 :comp {:phrasal false}}}
   :comp {:phrasal true
          :head {:phrasal false}
          :comp {:phrasal false}}})

(comment (repeatedly #(println (time (timeout-with consumer-patience generate-with-timeout)))))

;; Currently we can generate in 2.1 secs at 50% median, 2.4 secs max.
(def long-spec
  "e.g. 'the old students walk hands that the good studies would teach'"
  {:rule "s"
   :comp {:rule "np"
          :head {:phrasal true}}
   :head {:rule "vp"
          :head {:subcat {:1 {:cat :noun}
                          :2 {:cat :noun}
                          :3 []}}
          :comp {:rule "nbar4"
                 :phrasal true
                 :head {:phrasal false
                        :canonical :top}
                 :comp {:phrasal true
                        :rule "comp1"
                        :comp {:phrasal true
                               :rule "s-slash"
                               :comp {:phrasal true
                                      :rule "np"
                                      :head {:phrasal true}
                                      :comp {:phrasal false}}
                               :head {:phrasal true
                                      :rule "vp-aux-slash"
                                      :head {:phrasal false
                                             :aux true
                                             :subcat {:2 {:modal false}}}
                                      :comp {:phrasal (or false false) ;; changing this to true will take a long time and finally you'll get a 'dead end' error.
                                             :subcat {:1 {:cat :noun}}}}}}}}})
;; "what did tables hear?"
(def wh-spec-1
  {:rule "s-wh-interog"
   :head {:rule "s-interog-slash"
          :comp {:rule "s-comp-2"
                 :head {:phrasal false
                        :subcat {:2 {:cat :noun}}}
                 :comp {:phrasal false}}
          :head {:phrasal false}}
   :comp {:phrasal false}})

;; "at what did tables look?"
(def wh-spec-2
  {:rule "s-wh-interog"
   :head {:rule "s-interog-slash"
          :comp {:rule "s-comp-2"
                 :head {:phrasal false
                        :canonical "look"
                        :subcat {:2 {:cat :prep}}}}
          :head {:phrasal false}}})

(def wh-spec-3
  {:rule "s-wh-interog"
   :comp {:phrasal false}
   :head {:rule "s-interog-slash"
          :comp {:rule "s-comp-2"
                 :head {:rule "vp-slash"}}
          :head {:phrasal false}}})


;; TODO: "what did she look at?"
