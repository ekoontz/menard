(ns babel.generate.lab
  (:require [babel.generate :as g]
            [dag_unify.core :as u :refer [unify]]
            [clojure.string :as string]
            [clojure.tools.logging :as log]))

(def head-first
  (let [one (atom :top)
        two (atom :top)]
    {:phrasal true
     :head one
     :comp two
     :1 one
     :2 two}))

(def comp-first
  (let [one (atom :top)
        two (atom :top)]
    {:phrasal true
     :head two
     :comp one
     :1 one
     :2 two}))

(def baby-language
  {:grammar
   (->>
    [(let [cat-head (atom :n)
           cat-comp (atom :p)]
       ;; rule "X": phrase where both children are lexemes,
       ;; and both lexemes have share their :cat value and surface value.
       (unify {:cat cat-head
               :cat-comp cat-comp
               :rule "X"
               :head {:cat cat-head
                      :phrasal false}
               :comp {:cat cat-comp
                      :phrasal false}}
              comp-first))
     
     ;; rule "Y": phrase where the comp is a rule-"X".
     (let [cat-head (atom :top)
           comp-cat (atom :top)]
       (unify {:rule "Y"
               :cat cat-head
               :comp-cat comp-cat
               :head {:phrasal :top
                      :cat cat-head}
               :comp {:rule :top
                      :cat comp-cat
                      :phrasal true}}
              head-first))


     ;; rule "Z": phrase where the comp is a rule-"Y" and the head is some phrase.
     (unify {:rule "Z"
             :head {:rule "Y"
                    :phrasal true}
             :comp {:rule "X"
                    :phrasal true}}
            head-first)]
    
    (remove #(= :fail %)))

   :lexicon
   (into {} (for [[surface lexemes-for-surface]
                  {"ba" [{:cat :v}]
                   "da" [{:cat :n}]
                   "ga" [{:cat :p}]}]
              [surface (map (fn [lexeme]
                              (merge lexeme {:phrasal false
                                             :surface surface}))
                            lexemes-for-surface)]))})

(defn morph-ps [structure]
    (cond (or (= :fail structure) 
              (nil? structure)
              (string? structure)) structure

          (seq? structure)
          (map morph-ps structure)

          (u/get-in structure [:surface])
          (morph-ps (u/get-in structure [:surface]))

          (= false (u/get-in structure [:phrasal] false))
          "_"

          (u/get-in structure [:morph-ps])
          (:morph-ps structure)
          
          true
          (let [one (cond (= (get structure :1)
                             (get structure :head))
                          "*"
                          (= (get structure :1)
                             (get structure :comp))
                          "."

                          (= (get structure :2)
                             (get structure :head))
                          "."
                          (= (get structure :2)
                             (get structure :comp))
                          "*"

                          true
                          (throw (Exception. (str "the :1 is neither :head nor :comp: "
                                                  (type structure) "; empty: " (empty? structure) "; keys: "
                                                  (keys structure) "; "
                                                  (vec (:dag_unify.core/serialized structure))))))

                two (cond (= (get structure :2)
                             (get structure :head))
                          "*"
                          (= (get structure :2)
                             (get structure :comp))
                          "."
                          (= (get structure :1)
                             (get structure :head))
                          "."
                          (= (get structure :1)
                             (get structure :comp))
                          "*"
                          
                          true
                          (throw (Exception. (str "the :2 is neither :head nor :comp: "
                                                  (vec (:dag_unify.core/serialized structure))))))]

            (string/join ""
              (map morph-ps
                   ["[" (:rule structure)
                    (if (get structure :babel.generate/done?)
                      "+" " ")
                    " "
                    one (u/get-in structure [:1] "-") " "
                    two (u/get-in structure [:2] "-")
                    "]"])))))

(defn morph [structure]
  (cond (or (= :fail structure) 
            (nil? structure)
            (string? structure)) structure

        (seq? structure)
        (map morph structure)
        
        (u/get-in structure [:surface])
        (morph (u/get-in structure [:surface]))

        (= false (u/get-in structure [:phrasal] false))
        "_"
        
        true
        (string/join " "
                     (map morph
                          [(u/get-in structure [:1] "_")
                           (u/get-in structure [:2] "_")]))))

(defn generate [spec]
  (binding [g/grammar (shuffle (:grammar baby-language))
            g/lexicon (:lexicon baby-language)
            g/println? false
            g/morph-ps morph-ps
            g/default-fn (fn [x]
                           (if (and true (= true (u/get-in x [:babel.generate/done?])))
                             [(assoc-in x [:surface] (morph x))]
                             [x]))]
    (g/generate spec)))

(def the-spec {:rule "Z"})

(defn slowt []
  (binding [g/truncate? true]
    (generate the-spec)))

(defn slow []
  (binding [g/truncate? false]
    (generate the-spec)))

(defn demo []
  (do
    (println "five rule-X expressions:")
    (count (take 5 (repeatedly #(println (morph (generate {:rule "X"}))))))
    (println "five rule-Y expressions:")
    (count (take 5 (repeatedly #(println (morph (generate {:rule "Y"}))))))
    (println "five rule-Z expressions:")
    (count (take 5 (repeatedly #(println (morph (generate {:rule "Z"}))))))
    (println "five rule-X expressions (with structure):")
    (count (take 5 (repeatedly #(println (morph-ps (generate {:rule "X"}))))))
    (println "five rule-Y expressions (with structure):")
    (count (take 5 (repeatedly #(println (morph-ps (generate {:rule "Y"}))))))
    (println "five rule-Z expressions (with structure):")
    (count (take 5 (repeatedly #(println (morph-ps (generate {:rule "Z"}))))))))

