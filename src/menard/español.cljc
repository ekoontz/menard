(ns menard.español 
  (:require [menard.generate :as g]
            [menard.lexiconfn :as l]
            [menard.model :refer [create load-model]]
            [menard.morphology :refer [morph-leaf]]
            [menard.parse :as p]
            [menard.serialization :as s]
            [dag_unify.core :as u :refer [unify]]
            #?(:clj [clojure.java.io :as io :refer [resource]])
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(def model
  (ref (create "español/models/basic"
               "basic"
               (fn [lexicon _ _] lexicon)
               true)))

#?(:clj
   (defn syntax-tree [tree]
     (s/syntax-tree tree (-> model deref :morphology))))

(defn generate [spec]
  (log/info (str "generate: generating with: " spec))
  (let [model (deref model)
        retval
        (binding [g/max-depth (:max-depth spec g/max-depth)
                  g/max-fails (:max-fails spec g/max-fails)
                  g/allow-backtracking? true]
          (-> spec
              (g/generate (-> model :grammar)
                          (-> model :lexicon-index-fn)
                          syntax-tree)))]
    (log/info (str "generate: generated: " (-> retval syntax-tree)))
    retval))

(defn morph [tree]
   (cond
     ;; inflection doesn't work right, so disabling
     (and false

          (map? (u/get-in tree [:syntax-tree])))
     (morph (u/get-in tree [:syntax-tree]))

     :else
     (s/morph tree (:morphology @model))))

(defn convert-exceptions [exceptions]
  (concat (when (u/get-in exceptions [:present :1sing])
            [{:infl :present
              :agr {:person :1st :number :sing}
              :surface (u/get-in exceptions [:present :1sing])}])
          (when (u/get-in exceptions [:present :2sing])
            [{:infl :present
              :agr {:person :2nd :number :sing}
              :surface (u/get-in exceptions [:present :2sing])}])
          (when (u/get-in exceptions [:present :2sing])
            [{:infl :present
              :agr {:person :3rd :number :sing}
              :surface (u/get-in exceptions [:present :3sing])}])
          
          (when (u/get-in exceptions [:preterito :1sing])
            [{:infl :preterito
              :agr {:person :1st :number :sing}
              :surface (u/get-in exceptions [:preterito :1sing])}])
          (when (u/get-in exceptions [:preterito :2sing])
            [{:infl :preterito
              :agr {:person :2nd :number :sing}
              :surface (u/get-in exceptions [:preterito :2sing])}])
          (when (u/get-in exceptions [:preterito :2sing])
            [{:infl :preterito
              :agr {:person :3rd :number :sing}
              :surface (u/get-in exceptions [:preterito :3sing])}])))

(defn convert-stems [exceptions]
  (merge
   (when (u/get-in exceptions [:boot-stem])
     {:stems {:boot (u/get-in exceptions [:boot-stem])}})
   (when (u/get-in exceptions [:preterito-stem])
     {:stems {:preterito (u/get-in exceptions [:preterito-stem])}})))

(defn convert-unifies [v]
  (let [evalled (eval v)]
    (if (vector? evalled)
      (->> evalled
           (map (fn [v]
                  (apply unify (concat (get v :unify [:top])
                                       [(dissoc v :unify)])))))
      evalled)))

(defn convert []
  (->> (-> "resources/español/lexicon.edn"
           slurp
           read-string) 
       (map (fn [[k v]]
              [k (convert-unifies v)]))
       (map (fn [[k v]]
              [k (cond (vector? v)
                       v
                       (seq? v)
                       (vec v)
                       :else
                       [v])]))
       (map (fn [[k vs]]
              [k (->> vs
                      (map (fn [v]
                             (-> {}
                                 (merge (when (u/get-in v [:synsem :sem])
                                          {:sem (u/get-in v [:synsem :sem])}))
                                 (merge (when (u/get-in v [:synsem :cat])
                                          {:cat (u/get-in v [:synsem :cat])}))
                                 (merge (when (u/get-in v [:synsem :agr])
                                          {:agr (u/get-in v [:synsem :agr])}
                                          {}))
                                 (merge (if (u/get-in v [:espanol])
                                          (let [converted-exceptions (convert-exceptions (u/get-in v [:espanol]))]
                                            (if (not (empty? converted-exceptions))
                                              {:exceptions (vec converted-exceptions)}
                                              {}))))
                                 (merge (if (u/get-in v [:espanol])
                                          (let [stems (convert-stems (u/get-in v [:espanol]))]
                                            (if stems
                                              stems
                                              {}))))))))]))
       (map (fn [[k vs]]
              [k (vec vs)]))
       (filter (fn [[k v]]
                 (not (empty? (->> v
                                   (map #(u/get-in % [:cat]))
                                   (filter #(= :verb %)))))))
       (into {})))

;; for parsing diagnostics:
(def truncate? true)

;; how to split up a string into tokens that can be analyzed:
(def split-on #"[ ]+")

(defn analyze
  ([surface]
   (analyze surface false))
  ([surface use-null-lexemes?]
   (analyze surface false @model))
  ([surface use-null-lexemes? model]
   (log/info (str "analyze with model named: " (-> model :name) "; morphology size: " (count (vec (:morphology model)))))
   (let [variants (vec (set [(clojure.string/lower-case surface)
                             (clojure.string/upper-case surface)
                             (clojure.string/capitalize surface)]))
         lexicon (-> model :lexicon)
         morphology (:morphology model)
         found (mapcat (fn [variant]
                         (l/matching-lexemes variant lexicon morphology))
                       variants)]
     (log/info (str "found: " (count found) " for: [" surface "]"))
     (if (seq found)
       found
       (if (and use-null-lexemes?
                (not (clojure.string/includes? surface " ")))
         (let [found (l/matching-lexemes "_" lexicon morphology)]
           (log/debug (str "no lexemes found for: [" surface "]"
                           (when (seq found)
                             (str "; will use null lexemes instead."))))
           found))))))

(defn parse
  ([expression model]
   (let [model @model
         ;; remove trailing '.' if any:
         expression (clojure.string/replace expression #"[.]*$" "")

         ;; meaning of 1st arg passed to (analyze):
         ;; true: allow use of null lexemes
         analyze-fn-with-nulls #(analyze % true model)
         ;; false: DON'T allow use of null lexemes
         analyze-fn-without-nulls #(analyze % false model)

         ;; ^ TODO: should handle '.' and other punctuation like '?' '!' and
         ;; use it as part of the meaning
         ;; i.e.
         ;; '.' -> declarative
         ;; '?' -> interrogative
         ;; '!' -> imperative
         grammar (-> model :grammar)]
     (log/debug (str "calling p/parse with grammar: " (count grammar)))
     (let [parses-without-nulls
           (->> (p/parse expression grammar analyze-fn-without-nulls
                         syntax-tree morph split-on truncate?)
                (filter :complete?))]
       (log/debug (str "was without-null parses empty? " (empty? parses-without-nulls)))
       (if (seq parses-without-nulls)
         parses-without-nulls

         ;; else if no results ,try *with* nulls:
         (let [parses-with-nulls
               (->> (p/parse expression grammar analyze-fn-with-nulls
                             syntax-tree morph split-on truncate?)
                    (filter :complete?))]
           (log/debug (str "was with-null parses empty? " (empty? parses-with-nulls)))
           (if (seq parses-with-nulls)
             parses-with-nulls
             (let [parses-with-null-appended
                   (->> (p/parse (str expression " _") grammar analyze-fn-with-nulls
                                 syntax-tree morph split-on truncate?)
                        (filter :complete?))]
               (log/debug (str "was with-null-appended parses empty? " (empty? parses-with-null-appended)))
               parses-with-null-appended)))))))
  ([expression]
   (parse expression model)))

