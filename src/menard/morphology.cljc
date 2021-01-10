(ns menard.morphology
  (:require [clojure.string]
            [menard.exception :refer [exception]]
            [menard.lexiconfn :as l]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.diagnostics :as diag :refer [fail-path strip-refs]]))

(defn morph-leaf
  "Apply morphology to a leaf node of a tree: transform the leaf's canonical string into a
   an inflected string. The morphology is a set of rules, each of which has a :u and a :g. The :u is
   what to unify the structure against, and the :g contains a _from_ and a _to_, both of which
   are regular expressions used to transform the canonical form into the inflected form."
  [structure morphology]
  (log/debug (str "morph-leaf structure:" (diag/strip-refs structure)))
  (let [structure structure
        canonical (u/get-in structure [:canonical])
        inflected? (u/get-in structure [:inflected?] false)
        inflected? (if (= inflected? :top)
                     false
                     inflected?)
        matching-rules
        (when (or (not inflected?)
                (= :top inflected?))
          (filter (fn [rule]
                    (let [{u :u
                           [from _] :g
                           debug :debug} rule]
                      (and (string? canonical)
                           (re-find from canonical)
                           (let [result (unify u structure)]
                             (if (not (= :fail result))
                               (log/debug (str "success: u: " u " and structure: " (strip-refs structure) " and debug: " debug))
                               (log/debug (str "fail:     u: " u "; and structure: " (strip-refs structure) " : " (fail-path u structure) " and debug: " debug " and canonical: " canonical)))
                             (not (= :fail result))))))
                  morphology))
        exceptions (u/get-in structure [:exceptions])
        exceptionless (when exceptions
                        (dissoc structure :exceptions))
        first-matching-exception
        (when (and exceptions (not (keyword? exceptions))) ;; if :top, ignore.
          (first (filter #(not (= :fail %))
                          (map #(unify exceptionless %)
                               exceptions))))]
    (if first-matching-exception
      (log/debug (str "morph-leaf: " (diag/strip-refs first-matching-exception)))
      (log/debug (str "morph-leaf: no rules matched.")))
    (log/debug (str "morph-leaf: number of matching rules: " (count matching-rules)))
    (when (> (count matching-rules) 1)
      (log/debug (str "morph-leaf: more than one rule matched: " (diag/strip-refs structure) "; rules were: "
                      (->> (range 0 (count matching-rules))
                           (map (fn [i]
                                  (str "#" (+ 1 i) ": " (:u (nth matching-rules i)))))
                           (clojure.string/join ", ")))))
    (cond
      first-matching-exception
      (do
        (log/debug (str "found an exception: using that: " first-matching-exception))
        (morph-leaf first-matching-exception morphology))

      (u/get-in structure [:surface])
      (do
        (log/debug (str "found surface; using that: " (u/get-in structure [:surface])))
        (u/get-in structure [:surface]))

      (= true (u/get-in structure [:inflected?] false))
      (do
        (log/debug (str "found canonical; using that: " (u/get-in structure [:canonical])))
        (u/get-in structure [:canonical]))
      
      (and (false? inflected?) (empty? matching-rules)
           (not (= structure {:head? false}))
           (not (= structure {:head? true})))
      (do
        (log/warn (str "no rules matched: returning '_'"))
        "_")

      (not (seq? matching-rules))
      (exception (str "syntax error in matching rules: "
                      "should be a sequence but it's a: "
                      (type matching-rules)
                      " for matching: " (diag/strip-refs structure)))
      
      (seq matching-rules)
      (let [{[from to] :g} (first matching-rules)]
         (log/debug (str "using matching rule:" (first matching-rules)))
        (clojure.string/replace (u/get-in structure [:canonical] "")
                                from to))

      :else
      "_")))

;; Using a macro here for use by Clojurescript, so that
;; the Clojure (Java) side compiles it, since I haven't tried to get
;; the compilation working on Clojurescript, or maybe I tried and gave
;; up temporarily:
#?(:cljs
(defmacro compile-morphology [filenames]
  `(reduce
    concat
    ~(vec (map (fn [filename]
                 (l/read-and-eval filename)
                ;; allow a rule to be either a map (a single rule)
                ;; or a sequence of rules (the :else case below):
                (mapcat (fn [rule-or-rules]
                          (cond (map? rule-or-rules)
                                [rule-or-rules]
                                :else rule-or-rules))))
               filenames)))))

#?(:clj
(defn compile-morphology-fn [filenames]
  (reduce
   concat
   (vec (map (fn [filename]
               (->>
                (l/read-and-eval filename)
                ;; allow a rule to be either a map (a single rule)
                ;; or a sequence of rules (the :else case below):
                (mapcat (fn [rule-or-rules]
                          (cond (map? rule-or-rules)
                                [rule-or-rules]
                                :else rule-or-rules)))))

             filenames)))))
