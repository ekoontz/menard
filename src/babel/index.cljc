(ns babel.index
  (:refer-clojure :exclude [get-in resolve find parents])
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [dag_unify.core :refer [get-in unify]]))

;; TODO: these first two functions "map-subset-by-path"
;; and "map-subset-by-path2" are not descriptively named
;; and could be combined into less code. Replace with more idiomatic and
;; concise Clojure usage.
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
  (let [result
        (reduce intersection-with-identity
                (filter #(not (empty? %))
                        (map (fn [path]
                               (get (get indices path)
                                    (get-in spec path ::undefined)
                                    []))
                             index-lexicon-on-paths)))]
    (shuffle result)))



