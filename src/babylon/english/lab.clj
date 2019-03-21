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

