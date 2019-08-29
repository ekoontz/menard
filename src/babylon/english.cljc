(ns babylon.english
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [babylon.lexiconfn :as l]
            [babylon.generate :as g]
            [babylon.grammar :as grammar]
            [babylon.morphology :as m]
            [babylon.parse :as p]
            [babylon.serialization :as s]
            [babylon.ug :as ug]
            [clojure.tools.logging :as log]
            [dag_unify.core :as u :refer [unify]]))
;;
;; For generation and parsing of English.
;;
(defn apply-rules-to [lexicon]
  (-> lexicon
      (l/apply-rules-in-order (l/read-rules "babylon/english/lexicon/rules/rules-0.edn") :0)
      (l/apply-rules-in-order (l/read-rules "babylon/english/lexicon/rules/rules-1.edn") :1)
      (l/apply-rules-in-order (l/read-rules "babylon/english/lexicon/rules/rules-2.edn") :2)))

(defn compile-lexicon [filename]
  (-> filename
      l/read-rules
      l/add-exceptions-to-lexicon
      apply-rules-to))

(def lexicon
  (merge-with concat
    (compile-lexicon "babylon/english/lexicon/adjectives.edn")
    (compile-lexicon "babylon/english/lexicon/misc.edn")
    (compile-lexicon "babylon/english/lexicon/propernouns.edn")
    (compile-lexicon "babylon/english/lexicon/nouns.edn")
    (compile-lexicon "babylon/english/lexicon/verbs.edn")))

(def finite-tenses
  [;; "would see"
   {:variant :conditional
    :infl :present
    :sem {:tense :conditional}
    :head {:aux true}}

   ;; "will see"
   {:variant :future
    :infl :present
    :sem {:tense :future}
    :head {:aux true}}

   ;; "sees"
   {:variant :present-simple
    :abbreviation :simple-present
    :infl :present
    :modal false
    :sem {:tense :present
          :aspect :simple}}

   ;; "can see"
   {:variant :modal
    :infl :base
    :modal :base
    :sem {:tense :present
          :aspect :simple}}

   ;; "saw"
   {:variant :past
    :infl :past-simple
    :sem {:tense :past
          :aspect :simple}}

   ;; "is seeing"
   {:variant :present-progressive
    :infl :present
    :head {:aux true}
    :sem {:tense :present
          :aspect :progressive}}

   ;; "was seeing"
   {:variant :past-progressive
    :infl :past-simple    
    :head {:aux true}
    :sem {:tense :past
          :aspect :progressive}}

   ;; "has seen"
   {:variant :perfect
    :infl :present
    :head {:aux true}
    :sem {:tense :past 
          :aspect :perfect}}

   ;; "had seen"
   {:variant :pluperfect
    :infl :past-simple
    :head {:aux true}
    :sem {:tense :past
          :aspect :pluperfect}}])

(def slash-tenses ;; used for vp-aux-slash
  (->> finite-tenses
       (filter #(not (= :simple-present (u/get-in % [:abbreviation]))))))

(def tenses
  (concat finite-tenses
          [{:infl :base}
           {:infl :gerund}
           {:infl :past-participle}]))

(def aux-tenses
  [{:infl :present
    :sem {:tense :conditional}}
   {:infl :present
    :sem {:tense :future}}
   {:infl :present
    :sem {:tense :present
          :aspect :progressive}}
   {:infl :past-simple
    :sem {:tense :past
          :aspect :progressive}}
   {:infl :present
    :sem {:tense :past
          :aspect :perfect}}
   {:infl :past-simple
    :sem {:tense :past
          :aspect :pluperfect}}])

(def grammar
  (-> "babylon/english/grammar.edn"
      io/resource
      slurp
      read-string
      grammar/process))

(def morphology
  (concat
   (-> "babylon/english/morphology/nouns.edn"
       l/read-rules)
   (-> "babylon/english/morphology/verbs.edn"
       l/read-rules)))

(declare an)
(declare sentence-punctuation)

(defn morph
  ([tree]
   (cond
     (map? (u/get-in tree [:syntax-tree]))
     (s/morph (u/get-in tree [:syntax-tree]) morphology)

     true
     (s/morph tree morphology)))

  ([tree & {:keys [sentence-punctuation?]}]
   (if sentence-punctuation?
     (-> tree
         morph
         an
         (sentence-punctuation (u/get-in tree [:sem :mood] :decl))))))

(defn syntax-tree [tree]
  (cond (= false (u/get-in tree [:phrasal]))
        (morph tree)
        true
        (s/syntax-tree tree morphology)))

(defn an
  "change 'a' to 'an' if the next word starts with a vowel; 
   and change 'an' to 'a' if the next word does *not* start with a vowel."
  [input]
  (-> input
      (string/replace #"\b([aA]) ([aeiou])"  "$1n $2")
      (string/replace #"\b([aA])n ([^aeiou])" "$1 $2")))

(declare sentence-punctuation)

(defn sentence-punctuation
  "Capitalizes the first letter and puts a period (.) or question mark (?) at the end."
  [input mood]
  (str (string/capitalize (first input))
       (subs input 1 (count input))
       (if (= mood :interog)
         "?"
         ".")))

(defn lookup
  "find lexemes that satisfy _spec_."
  [spec]
  (binding [g/lexicon lexicon
            g/syntax-tree syntax-tree]
    (let [spec (let [with-subcat-empty
                     (unify spec {:subcat []})]
                 (if (= :fail with-subcat-empty)
                   spec
                   with-subcat-empty))]
       (g/get-lexemes spec))))

(def flattened-lexicon
  (flatten (vals lexicon)))

(def verb-lexicon
  (->> flattened-lexicon
       (filter #(and (not (u/get-in % [:exception]))
                     (= (u/get-in % [:cat]) :verb)))))
  
(def non-verb-lexicon
  (->> flattened-lexicon
         (filter #(and (not (= (u/get-in % [:cat]) :verb))
                       (not (u/get-in % [:exception]))))))


(defn index-fn [spec]
  (cond (= (u/get-in spec [:cat]) :verb)
        (shuffle verb-lexicon)
        true
        (shuffle non-verb-lexicon)))

(defn generate
  "generate one random expression that satisfies _spec_."
  [spec]
  (binding [g/grammar grammar
            g/lexicon lexicon
            g/syntax-tree syntax-tree
            g/index-fn index-fn]
    (-> spec
        g/generate)))

(defn generate-n
  "generate _n_ consecutive in-order expressions that satisfy _spec_."
  [spec n]
  (take n (repeatedly #(generate spec))))

(defn parse [expression]
  (binding [p/grammar grammar
            l/lexicon lexicon
            l/morphology morphology
            p/lookup-fn l/matching-lexemes]
    (p/parse expression syntax-tree morph)))

(defn analyze [surface]
  (binding [l/lexicon lexicon
            l/morphology morphology]
    (l/matching-lexemes surface)))              

(defn generate-long-sentence [& {:keys [spec trees]}]
  (if (empty? trees)
    (throw (Exception. (str "no trees left; can't generate a sentence.")))
   (let [extra-constraints (if spec spec :top)
         tree (unify extra-constraints (first trees))]
    (try (generate
          (unify {:cat :verb :subcat []
                  :sem {:mood :decl}}
                 tree))
         (catch Exception e
           (do (log/warn (str "failed to generate: "
                              (syntax-tree (:tree (ex-data e)))
                              " at path:" (:frontier-path (ex-data e))
                              " with spec: " (u/strip-refs (:child-spec (ex-data e))) 
                              "; retrying with same root: '" (u/get-in (:tree (ex-data e)) [:root])
                              "'.."))
               (generate-long-sentence
                :trees (rest trees)
                :spec {:root (u/get-in (:tree (ex-data e)) [:root])})))))))

(defn generate-retry [spec tries]
  (try (generate spec)
       (catch Exception e
         (do
           (log/warn (str "failed to generate: "                  
                          (syntax-tree (:tree (ex-data e))) " with spec:"
                          (u/strip-refs (:child-spec (ex-data e))) "; at path:"
                          (:frontier-path (ex-data e)) ";e=" (type e)))
           (if (> tries 0)
             (generate-retry spec (- tries 1))
             (throw e))))))

(defn demo []
  (println "Generation:")
  (println "===")
  (println)
  (println "= transitive sentences =")
  (println)
  (count (take 10 (repeatedly #(->
                                {:cat :verb
                                 :rule "s"
                                 :reflexive false
                                 :sem {:mood :decl
                                       :obj {:pred :top}}}
                                (generate-retry 3)
                                (morph :sentence-punctuation? true)
                                println))))

  (println)
  (println "= reflexive sentences =")
  (println)
  (count (take 10 (repeatedly #(->
                                {:cat :verb
                                 :rule "s"
                                 :reflexive true}
                                (generate-retry 3)
                                (morph :sentence-punctuation? true)
                                println))))
  (println)
  (println "= Interrogative sentences =")
  (println)
  (count
   (take 10 (repeatedly #(-> {:cat :verb
                              :sem {:mood :interog}}
                             (generate-retry 3)
                             (morph :sentence-punctuation? true)
                             println))))
  (println)
  (println "Parsing:")
  (println "===")
  (println)
  (count (take 10
               (repeatedly #(let [surface (morph (generate-retry {:cat (first (shuffle [:noun :verb]))
                                                                  :subcat []}
                                                                 3))]
                              (println (->> (parse surface)
                                            (map (fn [tree]
                                                   (binding [p/lookup-fn l/matching-lexemes]
                                                     (syntax-tree tree))))
                                            (string/join ", "))))))))

(defn capitalize-first-letter
  "clojure.string/capitalize is too much: it lower-cases every word in the string *except*
   the first letter. We need a function that capitalizes the first letter and leaves
   the rest alone (they might already be upper-case like the pronoun 'I', and should stay that way)."
  [input]
  (string/join ""
               (concat (string/capitalize (first input))
                       (rest input))))

(defn an
  "change 'a' to 'an' if the next word starts with a vowel."
  [input]
  (-> input
      (string/replace #"\b([aA]) ([aeiou])"   "$1n $2")
      (string/replace #"\b([aA])n ([^aeiou])" "$1 $2")))

(defn with-head [head & [rule]]
  (let [extended-info? false
        lexemes (analyze head)]
    (->> grammar
         (filter #(or (nil? rule) (= rule (u/get-in % [:rule]))))
         (map (fn [rule]
                (map (fn [lexeme]
                       (if (= false extended-info?)
                         (unify rule {:head lexeme})
                         
                         ;; show 'extended-info':
                         {:l lexeme
                          :r rule
                          :u (unify rule {:head lexeme})
                          :fp (u/fail-path rule {:head lexeme})}))
                     lexemes)))
         (reduce (fn [first rest]
                   (lazy-cat first rest)))

         ;; remove fails; need this 'or' because we
         ;; might be either in extended-info?-mode or not.
         (remove #(or (and extended-info?
                           (= :fail (:u %)))
                      (and (not extended-info?)
                           (= :fail %)))))))

(defn with-comp
  "analyze _comp_ as a lexeme and try to add it as the :comp in _tree_."
  [comp tree] 
  (let [extended-info? false
        lexemes (analyze comp)]
    (->> lexemes

         (map (fn [lexeme]
                (if (= false extended-info?)
                    (unify tree {:comp lexeme})
                    ;; show 'extended-info':
                    {:l lexeme
                     :u (unify tree {:comp lexeme})
                     :fp (u/fail-path tree {:comp lexeme})})))

         ;; remove fails; need this 'or' because we
         ;; might be either in extended-info?-mode or not.
         (remove #(or (and extended-info?
                           (= :fail (:u %)))
                      (and (not extended-info?)
                           (= :fail %)))))))

(defn make-tree [head comp]
  (->> (with-head head)
       (map (fn [tree]
              (with-comp comp tree)))
       (reduce (fn [first rest] (lazy-cat first rest)))))

(defn gi [path]
  (fn [structure]
    (u/get-in structure path)))

    
