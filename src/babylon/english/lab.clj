(ns babylon.english.lab
  (:require
   [babylon.english :as en :refer [analyze generate morph parse syntax-tree]]
   [babylon.generate :as g]
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

(def poetry-specs2
  [{:cat :verb
    :rule "s"
    :sem {:tense :present
          :aspect :progressive}
    :phrasal true
    :subcat []
    :comp {:phrasal true
           :rule "np"
           :head {:phrasal false}
           :comp {:phrasal false}}
    :head {:phrasal true
           :head {:phrasal false}
           :comp {:phrasal false}}}])

(defn poetry-line []
  (try
    (->
     poetry-specs2
     shuffle
     first
     (unify custom-spec)
     (unify {:top-level? true})
     generate)
    (catch Exception e
      (log/warn (str "failed to generate: "
                     (syntax-tree (:tree (ex-data e))) " with spec:"
                     (u/strip-refs (:child-spec (ex-data e))) "; at path:"
                     (:frontier-path (ex-data e)) ";e=" e))
      
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

(defn a-dog-were-seeing-a-cat []
  (let [spec
        {:cat :verb
         :reflexive false
         :agr {:number :sing}
         :sem {:mood :decl
               :pred :see
               :aspect :progressive
               :tense :past
               :subj {:pred :dog}
               :obj {:pred :cat}}
         :comp {:rule "np"
                :phrasal true}}]
    
    (count
       (take 100
             (repeatedly
               #(println
                  (let [expr
                         (generate
                            spec)]
                     (str "m:" (morph expr :sentence-punctuation? true)))))))))

