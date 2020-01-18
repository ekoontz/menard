(ns babylon.morphology
  (:require [babylon.exception :refer [exception]]
            [babylon.lexiconfn :as l]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.serialization :refer [serialize]]))

(defn morph-leaf
  "apply morphology to a leaf node of a tree; where
the morphology is a set of rules, each of which looks like:"
  [structure morphology]
  (log/debug (str "morph-leaf:" (u/strip-refs structure)))
  (let [matching-rules
        (if (or (not (u/get-in structure [:inflected?]))
                (= :top (u/get-in structure [:inflected?])))
          (filter (fn [rule]
                    (let [{u :u [from to] :g} rule
                          unified (unify u structure)]
                      (and (not (= :fail unified))
                           (string? (u/get-in structure [:canonical] :top))
                           (re-find from (str (u/get-in structure [:canonical] ""))))))
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
      (log/debug (str "morph-leaf: " (u/strip-refs first-matching-exception))))
    (cond
      first-matching-exception
      (morph-leaf first-matching-exception morphology)

      (u/get-in structure [:surface])
      (u/get-in structure [:surface])

      (= true (u/get-in structure [:inflected?] false))
      (u/get-in structure [:canonical])
      
      (nil? matching-rules)
      (exception (str "No rules matched for:"
                      (u/strip-refs structure)))

      (not (seq? matching-rules))
      (exception (str "syntax error in matching rules: "
                      "should be a sequence but it's a: "
                      (type matching-rules)
                      " for matching: " (u/strip-refs structure)))
      
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

(defmacro compile-morphology [filenames]
  `(reduce
    concat
    ~(vec (map (fn [filename]
                 (l/read-and-eval filename))
               filenames))))

#?(:clj
   (defn write-compiled-morphology [morphology write-to-file]
     (spit write-to-file (vec (map serialize morphology)))))

(defmacro read-compiled-morphology [filename]
  `~(-> filename
        clojure.java.io/resource
        slurp
        read-string))
