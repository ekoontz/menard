(ns babel.generate.lab
  (:require [babel.generate :as g]
            [dag_unify.core
             :as u :refer [strip-refs unify]]
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
    [(let [cat-head (atom :v)
           cat-comp (atom :p)]
       ;; rule "X": phrase where both children are lexemes,
       ;; and both lexemes have share their :cat value and surface value.
       (unify {:rule "X"
               :head {:cat cat-head
                      :phrasal false}
               :comp {:cat cat-comp
                      :phrasal false}}
              comp-first))
     
     ;; rule "Y": phrase where the comp is a rule-"X".
     (unify {:rule "Y"
             :head {:phrasal false
                    :cat :v}
             :comp {:rule "X"
                    :phrasal true}}
            head-first)


     ;; rule "Z": phrase where the comp is a rule-"Y" and the head is some phrase.
     (unify {:rule "Z"
             :head {:phrasal true}
             :comp {:rule "Y"
                    :phrasal true}}
            head-first)]
    
    (remove #(= :fail %)))

   :lexicon
   (into {} (for [[surface lexemes-for-surface]
                  {"ba" [{:cat :v}]
                   "da" [{:cat :n}]}]
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
        (let [one (cond (= (get structure :1)
                           (get structure :head))
                        "*"
                        (= (get structure :1)
                           (get structure :comp))
                        "."
                        true
                        (throw (Exception. (str "the :1 is neither :head nor :comp: "
                                                (vec (:dag_unify.core/serialized structure))))))
              
              two (cond (= (get structure :2)
                           (get structure :head))
                        "*"
                        (= (get structure :2)
                           (get structure :comp))
                        "."
                        true
                        (throw (Exception. (str "the :2 is neither :head nor :comp: "
                                                (vec (:dag_unify.core/serialized structure))))))]
          
          (string/join ""
            (map morph-ps
                 ["[" (:rule structure)
                  (if (get structure :babel.generate/done?)
                    "+" " ")
                  " "
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
                             [(assoc-in x [:surface] (morph x))]
                             [x]))]
    (g/generate spec)))

(defn slowt []
  (let [spec {:rule "Y"}]
    
    (binding [g/grammar (shuffle (:grammar baby-language))
              g/lexicon (:lexicon baby-language)
              g/truncate? true
              g/default-fn (fn [x]
                             (do
                               (log/debug (str "LAB/DEFAULT-FN: " (morph-ps x)))
                               [x]))
              g/morph-ps morph-ps]
      (g/generate spec))))

(defn slow []
  (let [spec {:rule "Y"}]
    (binding [g/grammar (shuffle (:grammar baby-language))
              g/lexicon (:lexicon baby-language)
              g/truncate? false
              g/default-fn (fn [x]
                             (do
                               (log/debug (str "LAB/DEFAULT-FN: " (morph-ps x)))
                               [x]))
              g/morph-ps morph-ps]
      (g/generate spec))))

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

;; bad-tree (below) generated by this.
(def foo (time (slow)))

(def bad-tree
 {:babel.generate/started? true,
  :comp
  {:phrasal true,
   :rule "X",
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
   :rule "X",
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
  :rule "Y"})

(defn prefix?
  "return true iff vector a is a prefix of vector b."
  [a b]
  (cond (empty? a) true
        (empty? b) false
        (= (first a) (first b))
        (prefix? (rest a) (rest b))
        true false))

(defn udissoc
  "dissoc the path _path_ from _s_, where _s_ is a serialized representation of a map with reentrances."
  [s path]
  (if (not (empty? s))
    (let [[paths m] (first s)]
      (cond (some #(prefix? path %) paths)
            :do-something
            (some #(prefix? % path) paths)
            :do-something-else
            true
            :do-the-default-thing))))

(defn dissoc-test []
  (let [post (u/dissoc-paths pre [[:comp :head]])]
    (morph-ps post)))

;; this is a partially-generated tree that looks like:
;; 
;;    Y
;;   / \
;; (H)  X
;;     / \
;;    C   H:ba 
;;
;; In the above, the root of the tree is {:rule "Y"}; the head "(H)"
;; means that the head has been truncated already, so
;; in the deserialized representation below, there is no
;; :head. Also there is no :1, because the rule "Y" is head-first,
;; so :head and :1 are identical.
;; 
;; The next step is to truncate the path [:comp :head], so that the tree will
;; look like:
;;
;;     Y
;;    / \
;;  (H)  X
;;      / \
;;     C  (H)
;;
;; In other words, there is no longer any of the following paths in the tree:
;;
;; [:comp :head]
;; [:2 :head]
;; [:comp :2]
;; [:2 :2]
;;
;; where all of these paths point to a single identical thing that has been truncated.

(def pre
  (u/deserialize
   '((nil
      {:babel.generate/started? true
       :comp :top
       :rule "Y"
       :phrasal true
       :2 :top})

     (((:comp) (:2))
      {:phrasal true
       :rule "X"
       :2 :top
       :1 :top
       :head :top
       :comp :top
       :babel.generate/started? true})

     (((:comp :2) (:comp :head) (:2 :2) (:2 :head))
      {:phrasal false
       :cat :top, :surface "ba", :babel.generate/done? true})

     (((:comp :1) (:comp :comp) (:2 :1) (:2 :comp))
      {:cat :top, :phrasal false})

     (((:comp :2 :cat) (:comp :head :cat) (:2 :2 :cat) (:2 :head :cat))
      :v)

     (((:comp :1 :cat) (:comp :comp :cat) (:2 :1 :cat) (:2 :comp :cat))
      :p))))

;; For now this is hand-generated until I can teach the computer to do
;; it:
(def post
  (u/deserialize
   '((nil
      {:babel.generate/started? true
       :comp :top
       :rule "Y"
       :phrasal true
       :2 :top})

     (((:comp) (:2))
      {:phrasal true
       :rule "X"
       :1 :top
       :comp :top
       :babel.generate/started? true})

     (((:comp :1) (:comp :comp) (:2 :1) (:2 :comp))
      {:cat :top
       :phrasal false})

     (((:comp :1 :cat) (:comp :comp :cat) (:2 :1 :cat) (:2 :comp :cat))
      :p))))

(defn find-matching-sets
  "given an structure and a path, determine its set of re-entrance-sets, and
   return the subset of those re-entrance-sets, such that in each 
   member of this subset, there is a path where _path_ is a 
   prefix of the path."
  [structure path]
  (let [reentrance-sets (map first (u/serialize pre))]
    (mapcat (fn [each-set]
               (and (some #(prefix? path %) each-set)
                    each-set))
            reentrance-sets)))

(defn good-dissoc [structure path]
  (u/dissoc-paths structure (find-matching-sets structure path)))
