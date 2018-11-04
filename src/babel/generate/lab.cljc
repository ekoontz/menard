(ns babel.generate.lab
  (:require [babel.generate :as g]
            [dag_unify.core
             :as u :refer [strip-refs unify]]
            [clojure.string :as string]))

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
    [(let [cat (atom :n)
           surface (atom :top)]
       ;; rule "A": phrase where both children are lexemes,
       ;; and both lexemes have share their :cat value and surface value.
       (unify {:rule "A"
               :head {:cat cat
                      :surface surface
                      :phrasal false}
               :comp {:cat cat
                      :surface surface
                      :phrasal false}}
              head-first))
     
     ;; rule "B": phrase where the comp is a rule-"A".
     (unify {:rule "B"
             :head {:phrasal false
                    :cat :v}
             :comp {:rule "A"
                    :phrasal true}}
            comp-first)


     ;; rule "C": phrase where the comp is a rule-"B" and the head is some phrase.
     (unify {:rule "C"
             :head {:phrasal true}
             :comp {:rule "B"
                    :phrasal true}}
            head-first)]
    
    (remove #(= :fail %)))

   :lexicon
   (into {} (for [[surface lexemes-for-surface]
                  {"ba" [{:cat :v}]
                   "da" [{:cat :n}]
                   "ga" [{:cat :v}]
                   "ma" [{:cat :n}]}]
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
        
        true
        (let [one (if (= (get structure :1)
                         (get structure :head))
                    "h:" "c:")
              two (if (= (get structure :1)
                         (get structure :head))
                    "c:" "h:")]
          (string/join ""
            (map morph-ps
                 ["[" (:rule structure) " "
                  one (u/get-in structure [:1] "_") " "
                  two (u/get-in structure [:2] "_")
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
                             [(-> x
                                  (assoc-in [:surface] (morph x))
                                  (u/dissoc-paths [[:1] [:2] [:head] [:comp]]))]
                             [x]))]
    (g/generate spec)))

(defn gen [spec]
  (binding [g/grammar (shuffle (:grammar baby-language))
            g/lexicon (:lexicon baby-language)
            g/println? false
            g/morph-ps morph-ps
            g/default-fn (fn [x]
                           (if (= true (u/get-in x [:babel.generate/done?]))
                             [(-> x
                                  (assoc-in [:surface] (morph x))
                                  (u/dissoc-paths [[:1] [:2] [:head] [:comp]]))]
                             [x]))]
    (g/gen spec)))

(defn slow []
  (let [spec {:rule "C"}]
    (binding [g/grammar (shuffle (:grammar baby-language))
              g/lexicon (:lexicon baby-language)
              g/default-fn (fn [x]
;;                             (println (str "default: " (morph-ps x)))
                             (cond

                               (or (= true (u/get-in x [:babel.generate/done?]))
                                   (and (= true (u/get-in x [:comp :babel.generate/done?]))
                                        (= true (u/get-in x [:head :babel.generate/done?]))))
                               [(-> x
                                    (assoc-in [:surface] (morph x))
                                    (assoc-in [:babel.generate/done?] true)
                                    (u/dissoc-paths [[:1] [:2] [:head] [:comp]]))]
                               
                               (and (= true (u/get-in x (concat (g/frontier x) [:babel.generate/done?])))
                                    false
                                    (= :comp (last (g/frontier x))))
                               (let [front (g/frontier x)]
                                 (println (str "front(1) " front " is done for: " (morph-ps x)))
                                 (println (str "tree: " (u/strip-refs x)))
                                 [(-> x
                                      (u/dissoc-paths [front
                                                       (concat (butlast front) [:head :comp])
                                                       (concat (butlast front) [:head :head])]))])
                               
                               (= true (u/get-in x (concat (g/frontier x) [:babel.generate/done?])))
                               (let [front (g/frontier x)]
                                 (println (str "frontier: " front " is unexpectedly done for: " (morph-ps x)))
;;                                 (println (str "tree: " (u/strip-refs x)))
                                 [(-> x
                                      (u/dissoc-paths [front]))])

                               true
                               (do
                                 [x])))
              g/morph-ps morph-ps]
      (g/generate spec))))

(defn demo []
  (do
    (println "five rule-A expressions:")
    (count (take 5 (repeatedly #(println (morph (generate {:rule "A"}))))))
    (println "five rule-B expressions:")
    (count (take 5 (repeatedly #(println (morph (generate {:rule "B"}))))))
    (println "five rule-C expressions:")
    (count (take 5 (repeatedly #(println (morph (generate {:rule "C"}))))))
    (println "five rule-A expressions (with structure):")
    (count (take 5 (repeatedly #(println (morph-ps (generate {:rule "A"}))))))
    (println "five rule-B expressions (with structure):")
    (count (take 5 (repeatedly #(println (morph-ps (generate {:rule "B"}))))))
    (println "five rule-C expressions (with structure):")
    (count (take 5 (repeatedly #(println (morph-ps (generate {:rule "C"}))))))))

;; bad-tree (below) generated by this.
(def foo (time (slow)))

(def bad-tree
 {:babel.generate/started? true,
  :comp
  {:phrasal true,
   :rule "A",
   :2
   {:phrasal false, :surface "ma", :cat :n, :babel.generate/done? true},
   :1
   {:phrasal false, :surface "ma", :cat :n, :babel.generate/done? true},
   :head
   {:phrasal false, :surface "ma", :cat :n, :babel.generate/done? true},
   :comp
   {:phrasal false, :surface "ma", :cat :n, :babel.generate/done? true},
   :babel.generate/started? true,
   :babel.generate/done? true},
  :head
  {:cat :v, :phrasal false, :surface "ba", :babel.generate/done? true},
  :1
  {:phrasal true,
   :rule "A",
   :2
   {:phrasal false, :surface "ma", :cat :n, :babel.generate/done? true},
   :1
   {:phrasal false, :surface "ma", :cat :n, :babel.generate/done? true},
   :head
   {:phrasal false, :surface "ma", :cat :n, :babel.generate/done? true},
   :comp
   {:phrasal false, :surface "ma", :cat :n, :babel.generate/done? true},
   :babel.generate/started? true,
   :babel.generate/done? true},
  :2
  {:cat :v, :phrasal false, :surface "ba", :babel.generate/done? true},
  :phrasal true,
  :rule "B"})

(def output "
frontier: (:head :head :head :comp :comp) is unexpectedly done for: [C h:[C h:[C h:[C h:[A h:da c:da] c:[B c:[A h:ma c:ma] h:ba]] c:[B h:_ c:_]] c:[B h:_ c:_]] c:[B h:_ c:_]]
frontier: (:head :head :head :comp) is unexpectedly done for: [C c:[C h:[C h:[C h:[A h:da c:da] c:[B c:[A h:ma c:ma] h:ba]] c:[B h:_ c:_]] c:[B h:_ c:_]] h:[B h:_ c:_]]
frontier: (:head :head :comp :comp) is unexpectedly done for: [C c:[C h:[C h:[C h:[A h:da c:da] c:[B c:[A h:ma c:ma] h:ba]] c:[B c:[A h:ma c:ma] h:ga]] c:[B h:_ c:_]] h:[B h:_ c:_]]
frontier: (:head :head :comp) is unexpectedly done for: [C c:[C h:[C h:[C h:[A h:da c:da] c:[B c:[A h:ma c:ma] h:ba]] c:[B c:[A h:ma c:ma] h:ga]] c:[B h:_ c:_]] h:[B h:_ c:_]]
frontier: (:head :comp :comp) is unexpectedly done for: [C c:[C h:[C h:[C h:[A h:da c:da] c:[B c:[A h:ma c:ma] h:ba]] c:[B c:[A h:ma c:ma] h:ga]] c:[B c:[A h:da c:da] h:ba]] h:[B h:_ c:_]]
frontier: (:head :comp) is unexpectedly done for: [C c:[C h:[C h:[C h:[A h:da c:da] c:[B c:[A h:ma c:ma] h:ba]] c:[B c:[A h:ma c:ma] h:ga]] c:[B c:[A h:da c:da] h:ba]] h:[B h:_ c:_]]
frontier: (:comp :comp) is unexpectedly done for: [C c:[C h:[C h:[C h:[A h:da c:da] c:[B c:[A h:ma c:ma] h:ba]] c:[B c:[A h:ma c:ma] h:ga]] c:[B c:[A h:da c:da] h:ba]] h:[B c:[A h:ma c:ma] h:ga]]
")
