(ns babel.index
  (:refer-clojure :exclude [get-in resolve find parents])
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [dag_unify.core :refer [fail? dissoc-paths get-in label-of
                           strip-refs unify]]))

(defn exception [error-string]
  #?(:clj
     (throw (Exception. error-string)))
  #?(:cljs
     (throw (js/Error. error-string))))

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


