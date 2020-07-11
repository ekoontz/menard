(ns menard.morphology
  (:require [menard.exception :refer [exception]]
            [menard.lexiconfn :as l]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.diagnostics :as diag :refer [fail-path strip-refs]]
            [dag_unify.serialization :refer [serialize]]))

(defn morph-leaf
  "apply morphology to a leaf node of a tree; where
the morphology is a set of rules, each of which looks like:"
  [structure morphology]
  (log/debug (str "morph-leaf structure:" (diag/strip-refs structure)))
  (let [structure structure
        canonical (u/get-in structure [:canonical])
        inflected? (u/get-in structure [:inflected?])
        matching-rules
        (if (or (not inflected?)
                (= :top inflected?))
          (filter (fn [rule]
                    (let [{u :u [from to] :g} rule]
                      (and (string? canonical)
                           (re-find from canonical)
                           (let [result (unify u structure)]
                             (if (not (= :fail result))
                               (log/debug (str "success: " u " and structure: " (strip-refs structure)))
                               (log/debug (str "fail:    " u " and structure: " (strip-refs structure) " : " (fail-path u structure))))
                             (not (= :fail result))))))
                  morphology))
        exceptions (u/get-in structure [:exceptions])
        exceptionless (if exceptions
                        (dissoc structure :exceptions))
        first-matching-exception
        (if (and exceptions (not (keyword? exceptions))) ;; if :top, ignore.
          (first (filter #(not (= :fail %))
                          (map #(unify exceptionless %)
                               exceptions))))]
    (if first-matching-exception
      (log/debug (str "morph-leaf: " (diag/strip-refs first-matching-exception))))
    (cond
      first-matching-exception
      (morph-leaf first-matching-exception morphology)

      (u/get-in structure [:surface])
      (u/get-in structure [:surface])

      (= true (u/get-in structure [:inflected?] false))
      (u/get-in structure [:canonical])
      
      (nil? matching-rules)
      (exception (str "No rules matched for:"
                      (diag/strip-refs structure)))

      (not (seq? matching-rules))
      (exception (str "syntax error in matching rules: "
                      "should be a sequence but it's a: "
                      (type matching-rules)
                      " for matching: " (diag/strip-refs structure)))
      
      (not (empty? matching-rules))
      (let [{[from to] :g} (first matching-rules)]
         (log/debug (str "using matching rule:" (first matching-rules)))
        (clojure.string/replace (u/get-in structure [:canonical] "")
                                from to))

      (= :top (u/get-in structure [:canonical]))
      "_"
      
      (u/get-in structure [:canonical])
      (u/get-in structure [:canonical])
      true
      "_")))

;; Using a macro here for use by Clojurescript, so that
;; the Clojure (Java) side compiles it, since I haven't tried to get
;; the compilation working on Clojurescript, or maybe I tried and gave
;; up temporarily:
(defmacro compile-morphology [filenames]
  `(reduce
    concat
    ~(vec (map (fn [filename]
                 (l/read-and-eval filename))
               filenames))))

(defn compile-morphology-fn [filenames]
  (reduce
   concat
   (vec (map (fn [filename]
               (l/read-and-eval filename))
             filenames))))

