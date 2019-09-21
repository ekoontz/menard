(ns babylon.english.lab
  (:require
   [babylon.english :as en :refer [analyze generate grammar index-fn lexicon
                                   morph parse syntax-tree]]
   [babylon.generate :as g :refer [add lazy-map]]
   [dag_unify.core :as u]
   [clojure.tools.logging :as log]))

(def long-demo-spec
  (-> {:rule "s"
       :comp {:rule "np"
              :head {:rule "nbar"}}
       :head {:rule "vp-aux"
              :comp {:rule "vp"
                     :comp {:rule "np"
                            :head {:rule "nbar"}}}}}))
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
    :head {:rule "vp-modal-2"
           :comp {:phrasal true}}}])

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

(def poetry-specs
  [
   {:rule "s"
    :cat :verb
    :phrasal true
    :subcat []
    :comp {:phrasal false}
    :head {:phrasal true
           :comp {:phrasal true}}}

   {:rule "s"
    :subcat []
    :cat :verb
    :comp {:phrasal true}}

   {:rule "s-interog"
    :cat :verb
    :subcat []
    :comp {:phrasal true}
    :sem {:mood :interog}}

   {:rule "s"
    :comp {:rule "np"}
    :head {:rule "vp"
           :head {:infl :present}
           :comp {:rule "nbar4"
                  :phrasal true
                  :comp {:top :top
                         :phrasal true
                         :rule "comp1"
                         :comp {:top :top
                                :phrasal true
                                :rule "s-slash"
                                :head {:top :top
                                       :rule "vp-aux-slash"}}}}}}
   {:rule "s"
    :comp {:rule "np"
           :head {:rule "nbar2"
                  :comp {:rule "s-slash"}}}
    
    :head {:phrasal true
           :comp {:phrasal true}}}])

(defn poetry-line []
  (try
    (->
     poetry-specs
     shuffle
     first
     generate)
    (catch Exception e
      (log/warn (str "fail:(" (-> e ex-data :why) ":)"
                     (syntax-tree (:tree (ex-data e))) " with spec:"
                     (u/strip-refs (:child-spec (ex-data e))) "; at path:"
                     (:frontier-path (ex-data e)) "; immediate-parent: "
                     (-> e ex-data :immediate-parent))))))

(defn benchmark []
  (repeatedly
   #(time (->
           (or (poetry-line) "(failed)")
           morph
           println))))

(defn poetry []
  (loop []
    (println (morph (or (poetry-line) "(failed)") :sentence-punctuation? true))
    (recur)))

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



