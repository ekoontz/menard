(ns babylon.english.lab
  (:require
   [babylon.english :as en :refer [analyze generate grammar index-fn lexicon
                                   morph parse syntax-tree]]
   [babylon.generate :as g :refer [add lazy-mapcat]]
   [dag_unify.core :as u]
   [clojure.tools.logging :as log]))

(def specs
  [{:phrasal true
    :rule "np"
    :head {:rule "nbar4"
           :phrasal true
           :comp {:phrasal true
                  :comp {:phrasal true
                         :comp {:phrasal true}}}}}
   {:phrasal true
    :rule "s"
    :comp {:phrasal true
           :agr {:number :sing}
           :sem {:pred :dog}}
    :canonical "be"}])

(def quick-demo-spec
  {:rule "s" :comp {:phrasal false} :head {:phrasal false}})

(def medium-demo-spec
  (-> {:rule "s"
       :comp {:rule "np"
              :head {:rule "nbar"}}
       :head {:rule "vp"
              :comp {:rule "np"
                     :head {:phrasal false}}}}))
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

(defn medium-timed-demo []
  (repeatedly
   #(println
     (-> medium-demo-spec
         generate
         time
         syntax-tree))))

(defn quick-demo []
  (repeatedly #(println (morph (time (generate quick-demo-spec))))))

;; for debugging generation and fixing grammatical rules
(defn partial-generate-test []
  (->> [{:rule "s"
         :comp {:phrasal false}
         :head {:rule "vp-modal-2"
                :comp {:phrasal true
                       :modal false
                       :head {:rule "vp"}}}}]
       (lazy-mapcat add)
       (lazy-mapcat add)
       (lazy-mapcat add)
       (lazy-mapcat add)
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
           :head {:canonical "move"
                  :infl :present}
           :comp {:rule "nbar4"
                  :head {:canonical "mother"}
                  :comp {:top :top
                         :rule "comp1"
                         :head {:top :top
                                :canonical "that"}
                         :comp {:top :top
                                :rule "s-slash"
                                :head {:top :top
                                       :rule "vp-aux-slash"}}}}}}])

(defn poetry-line []
  (try
    (->
     poetry-specs
;;     shuffle
     ;;     first
     last
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
           {:cat :verb
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
