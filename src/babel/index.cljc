(ns babel.index
  (:refer-clojure :exclude [get-in resolve find parents])
  (:require
   ;; TODO: comment is misleading in that we never call core/get-in from this file.
   ;; TODO: alphabetize
   [clojure.string :as string]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [dag_unify.core :refer [fail? dissoc-paths get-in label-of
                           strip-refs unify]]))

(defn exception [error-string]
  #?(:clj
     (throw (Exception. error-string)))
  #?(:cljs
     (throw (js/Error. error-string))))

(declare show-spec)

;; TODO: remove: not used anymore.
;; TODO: diagnostic function that is too specific currently (e.g. refers to ':english').
(defn check-index [index]
  (if (not (= :top (get-in (first (:head (get index "nbar"))) [:english :agr :number])))
    (throw (exception (str "CHECK INDEX FAILED! " (get index "nbar"))))))
  
;; TODO: remove: not used anymore.
(defn build-lex-sch-index [phrases lexicon all-phrases]
  "Build a mapping of phrases onto subsets of the lexicon. The two values (subsets of the lexicon) to be
   generated for each key (phrase) are: 
   1. the subset of the lexicon that can be the head of this phrase.
   2. the subset of the lexicon that can be the complement of this phrase.

   End result is a set of phrase => {:comp subset-of-lexicon 
                                     :head subset-of-lexicon}."
  (log/debug (str "build-lex-sch-index: lexicon size: " (count lexicon)))
  (log/debug (str "build-lex-sch-index: grammar size: " (count all-phrases)))
  (if (not (empty? phrases))
    (conj
     {(get-in (first phrases) [:rule])
      {:comp
       (filter (fn [lex]
                 (not (fail? (unify (first phrases)
                                     {:comp lex}))))
               lexicon)

       :comp-phrases
       (filter (fn [comp-phrase]
                 (not (fail? (unify (first phrases)
                                     {:comp comp-phrase}))))
               all-phrases)

       :head-phrases
       (filter (fn [head-phrase]
                 (not (fail? (unify (first phrases)
                                     {:head head-phrase}))))
               all-phrases)

       :head
       (filter (fn [lex]
                 (log/debug (str "trying lexeme: " lex))
                 (not (fail? (unify (first phrases)
                                     {:head lex}))))
               lexicon)}}
     (build-lex-sch-index (rest phrases) lexicon all-phrases))))

(defn show-spec [spec]
  (cond (seq? spec)
        (map show-spec spec)
        true
        (strip-refs (dissoc-paths spec '((:english :initial)
                                         (:italiano :initial)
                                         (:synsem :essere)
                                         (:synsem :agr)
                                         (:synsem :pronoun)
                                         (:synsem :sem :tense)
                                         (:synsem :sem :obj :tense)
                                         (:synsem :sem :mod)
                                         (:synsem :infl))))))

(defn map-subset-by-path2 [vals-at-path lexemes path]
  (if (not (empty? vals-at-path))
    (let [val (first vals-at-path)]
      (merge {val
              (filter (fn [lexeme]
                        (or (= :top (get-in lexeme path :top))
                            (= val
                               (get-in lexeme path))))
                      lexemes)}
             (map-subset-by-path2 (rest vals-at-path)
                                  lexemes
                                  path)))))

;; TODO: these functions "map-subset-by-path" are
;; not descriptive. Replace with more idiomatic and
;; concise Clojure usage.
(defn map-subset-by-path [lexicon path]
  (map-subset-by-path2
   (vec (set (filter #(not (= :top %))
                     (map (fn [entry]
                            (get-in entry path :top))
                          (flatten (vals lexicon))))))
   (flatten (vals lexicon))
   path))

(defn create-indices [lexicon index-lexicon-on-paths]
  (into {}
        (map (fn [path]
               [path (map-subset-by-path lexicon path)])
             index-lexicon-on-paths)))

(defn intersection-with-identity [ & [set1 set2]]
  (if (> (count set1)
         (count set2))
    (filter (fn [member2]
              (some (fn [member1]
                      (identical? member1 member2))
                    set1))
            set2)
    (filter (fn [member1]
              (some (fn [member2]
                      (identical? member1 member2))
                    set2))
            set1)))

(defn lookup-spec [spec indices index-lexicon-on-paths]
  (log/debug (str "index-fn called with spec: " 
                  (strip-refs
                   (dissoc (strip-refs spec)
                           :dag_unify.core/serialized))))
  (let [result
        (reduce intersection-with-identity
                (filter #(not (empty? %))
                        (map (fn [path]
                               (let [result
                                     (get (get indices path)
                                          (get-in spec path ::undefined)
                                          [])]
                                 (if (not (empty? result))
                                   (log/trace (str "subset for path:" path " => " (get-in spec path ::undefined)
                                                   " = " (count result)))
                                   (log/trace (str "empty result for path: " path "; spec=" (strip-refs spec))))
                                 result))
                             index-lexicon-on-paths)))]
    (log/debug (str "indexed size returned: " (count result) " for spec: " (strip-refs spec)))
    (if (and false (empty? result))
      (throw (Exception. (str "lookup-spec failed: " (strip-refs spec)))))
    (shuffle result)))


