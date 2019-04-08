(ns babylon.english.lab
  (:require
   [babylon.english :as en :refer [analyze generate morph parse syntax-tree]]
   [dag_unify.core :as u :refer [unify]]
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

   {:cat :verb
    :subcat []
    :sem {:mood :decl}
    :phrasal true}

   {:cat :verb
    :subcat []
    :rule "s-interog"
    :sem {:mood :interog}
    :phrasal true}
   
   {:cat :verb
    :subcat []
    :phrasal true
    :head {:phrasal true}}

   {:cat :verb
    :subcat []
    :phrasal true
    :comp {:phrasal true}}

   {:cat :verb
    :subcat []
    :comp {:phrasal true}
    :phrasal true
    :head {:phrasal true}}

   {:cat :verb
    :subcat []
    :phrasal true
    :comp {:phrasal true
           :head {:phrasal true}}
    :head {:phrasal true}}

   {:cat :verb
    :phrasal true
    :subcat []
    :comp {:phrasal true
           :head {:phrasal true}}
    :head {:phrasal true
           :comp {:phrasal true}}}

   {:cat :verb
    :phrasal true
    :subcat []
    :comp {:phrasal true
           :head {:phrasal true}}
    :head {:phrasal true
           :comp {:phrasal true
                  :comp {:phrasal true}}}}])

;; enable to generate with a part.
(def custom-spec
  (if false
    {:sem {:tense :future}}
    :top))

(defn poetry-line []
  (try
    (->
     poetry-specs
     shuffle
     first
     (unify custom-spec)
     (unify {:top-level? true})
     generate)
    (catch Exception e
      (log/debug (str "failed to generate: "
                     (syntax-tree (:tree (ex-data e))) " with spec:"
                     (u/strip-refs (:child-spec (ex-data e))) "; at path:"
                     (:frontier-path (ex-data e))))
      
      (poetry-line))))

(defn benchmark []
  (repeatedly
   #(time (->
           (poetry-line)
           (morph :sentence-punctuation? true)
           println))))

(defn poetry []
  (repeatedly
   #(-> 
     (or (poetry-line) "(failed)")
     (morph :sentence-punctuation? true)
     println)))

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

(defn bug-in-have-aux []
  (count
   (take 100
         (repeatedly
          #(println
            (let [expr
                  (generate
                   {:cat :verb
                    :reflexive false
                    :sem {:mood :decl
                          :pred :use
                          :aspect :progressive
                          :tense :past
                          :subj {:pred :they}}})]
              (str "m:" (morph expr
                               :sentence-punctuation? true)
                   "; sem:" (u/strip-refs (u/get-in expr [:sem])))))))))


;; thanks to Symfrog @ https://stackoverflow.com/a/26215960/9617245
;; this one has bugs on big outputs like the "food"
;; graphs generated below.
(defn dissoc-in-symfrog [m p]
  (if (get-in m p) 
    (update-in m
               (butlast p)
               dissoc (last p))
    m))

;; https://github.com/weavejester/medley/blob/1.1.0/src/medley/core.cljc#L20
(defn dissoc-in
  "Dissociate a value in a nested associative structure, identified by a sequence
  of keys. Any collections left empty by the operation will be dissociated from
  their containing structures."
  ([m ks]
   (if-let [[head & tail] ks]
     (if tail
       (let [v (dissoc-in (get m head) tail)]
         (if (empty? v)
           (dissoc m head)
           (assoc m head v)))
       (dissoc m head))
     m)))

(defn truncate-within [m p]
  (if (empty? p)
     (-> (reduce (fn [m path]
                   (dissoc-in m path))
                 m
                 [[:comp] [:1]
                  [:head] [:2]])
         (assoc :surface (morph m))
         (assoc :syntax-tree (syntax-tree m)))
    (let [truncated-within (truncate-within (u/get-in m p) [])
          truncate-from (u/get-in m (butlast p))]
      (swap! (get truncate-from (last p)) (fn [x] truncated-within))
      m)))

(defn truncate-test []
  (let [foo
        (generate
         {:cat :verb
          :reflexive false
          :sem {:mood :decl
                :pred :use
                :aspect :simple
                :tense :present
                :subj {:pred :they}}})
        truncated (truncate-within foo [])]
    (println (str "not truncated: " (syntax-tree foo)
                  "; size: " (count (str foo))))
    (println (str "truncated    : " (syntax-tree truncated)
                  "; size: " (count (str truncated))))))

(defn truncate-within-test []
  (let [spec
        {:cat :verb
         :reflexive false
         :sem {:mood :decl
               :pred :use
               :aspect :progressive
               :tense :past
               :subj {:pred :they}}}]
    (let [generated (generate spec)]
      (println (str "not truncated: " (syntax-tree generated)
                    "; size: " (count (str generated))))
      (let [do-truncation (truncate-within generated [:head])]
        (println (str "truncated:     " (syntax-tree generated)
                      "; size: " (count (str generated))))
        (let [final (truncate generated)]
          (println (str "truncated:     " (syntax-tree final)
                        "; size: " (count (str final)))))))))

(defn truncate-within-test-2 []
  (let [spec
        {:cat :verb
         :comp {:phrasal true
                :rule "np"}
         :reflexive false
         :sem {:mood :decl
               :pred :walk
               :aspect :progressive
               :tense :past
               :subj {:pred :dog}}}]
    (let [generated (generate spec)]
      (println (str "not truncated: " (syntax-tree generated)
                    "; size: " (count (str generated))))
      (let [truncated
            (-> generated
                (truncate-within [:head])
                (truncate-within [:comp])
                (truncate-within []))]
        (println (str "truncated:     " (syntax-tree truncated)
                      "; size: " (count (str truncated))))))))

