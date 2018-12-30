(ns babylon.lexiconfn
  (:require
   [babylon.exception :refer [exception]]
   [clojure.tools.logging :as log]
   [dag_unify.core :as u :refer [unify]]))

;; These functions are used to a convert human-friendly lexicon
;; into a machine-friendly data structure.
(defn apply-rules [rules lexemes]
  (let [one-round
        (->> rules
             (mapcat
              (fn [rule]
                (let [[antecedent consequent] rule]
                  (mapcat (fn [lexeme]
                             (cond
                               (fn? consequent)
                               ;; 1. _consequent_ can be a function that
                               ;; takes a map and returns a sequence of maps..
                               (let [result (consequent lexeme)]
                                 (if (empty? result)
                                   [lexeme]
                                   result))
                               true
                               ;; 2. ..or (the more frequent case)
                               ;; _consequent_ can be another map that
                               ;; we unify against the lexeme.
                               (let [result (unify lexeme consequent)]
                                 (cond (not (= :fail result))
                                       [result]
                                       true
                                       [lexeme]))))
                          lexemes)))))]
    (if (= (count (set one-round))
           (count (set (vec (clojure.set/union lexemes one-round)))))
      ;; we are done.
      lexemes
      ;; set of lexemes changed as a result of applying rules,
      ;; so re-apply rules:
      (apply-rules rules one-round))))

;; TODO: for now _rules_ assumes one-to-one rules:
;; (source lexeme -> compiled lexeme),
;; but should also support one-to-many rules:
;; (source lexeme -> collection(compiled lexeme).
(defn process [lexicon rules]
  (into {} (for [[surface lexemes-for-surface]
                 lexicon]
             [surface
              (->> lexemes-for-surface
                   (map (fn [lexeme]
                          (merge lexeme {:phrasal false
                                         :surface surface})))
                   (apply-rules rules))])))

(defn map-function-on-map-vals [m f]
     (if (and (not (map? m))
              (not (nil? m)))
         (throw (exception (str "Expected map as first input to map-function-on-map-vals, but got an input of type: " (type m)))))
   ;; TODO: add check for uniformity of type of keys
   ;; i.e. check that they are either all strings, or all keywords, or all integers, etc.
   ;; this is to avoid the need to log/debug below.
  (into {}
        (for [[k v] m]
          ;; for each <k,v> pair, return a <k,v'>, where v' = f(v).
          [k (f k v)])))
   
;; TODO: s/unify/unify!/ for performance
;; TODO: use (default) rather than this; it's simpler to understand and faster.
(defn if-then [lexicon if-has unify-with]
  (map-function-on-map-vals
   lexicon
   (fn [k vals]
     ;; TODO: use (map .. x) rather than (mapcat ... (list x))
     (mapcat (fn [val]
               (let [result (unify val if-has)]
                 (cond (not (= :fail result))
                       (do
                         (log/debug (str val ": matches: if: " if-has " then " unify-with))
                         (list (unify val unify-with)))
                       true
                       (list val))))
             vals))))

(defn constrain-vals-if
  "for each lexeme <k,v>, if v matches if-fn, then unify value with (then-fn v)."
  [lexicon if-fn then-fn]
  (into {}
        (map (fn [k]
               [k
                (map
                 (fn [val]
                   (cond (if-fn val)
                         (do
                           (log/debug (str val ": matches: constrain-vals:"
                                           (if-fn val)))
                           (unify val (then-fn val)))
                         
                         true
                         val))
                 (get lexicon k))])
             (keys lexicon))))


(defn filter-keys [lexicon filter-fn]
  (into {}
        (map (fn [k]
               [k (get lexicon k)])
             (filter filter-fn (keys lexicon)))))

(defn filter-vals [lexicon filter-fn]
  (into {}
        (map (fn [k]
               [k (filter filter-fn (get lexicon k))])
             (keys lexicon))))

(defn remove-vals [lexicon remove-fn]
  (into {}
        (map (fn [k]
               [k (remove remove-fn (get lexicon k))])
             (keys lexicon))))

(defn rewrite-keys [lexicon rewrite-fn]
  (into {}
        (map (fn [k]
               [(rewrite-fn k)
                (get lexicon k)])
             (keys lexicon))))

(defn get-fail-path [map1 map2]
  (if (and (map? map1)
           (map? map2))
    (let [keys1 (keys map1)
          keys2 (keys map2)
          fail-keys (mapcat (fn [key]
                              (if (= :fail
                                     (unify (get-in map1 [key] :top)
                                            (get-in map2 [key] :top)))
                                (list key)))
                            (set (concat keys1 keys2)))]
      (let [first-fail-key (first fail-keys)]
        (if (not (empty? fail-keys))
          (cons
           first-fail-key (get-fail-path (get-in map1 [first-fail-key])
                                         (get-in map2 [first-fail-key])))
          (if (not (nil? first-fail-key))
            [first-fail-key]))))))

(defn lexicon-for-generation [lexicon]
  "filter elements of a lexicon that are not intended for generation (:use-for-generation=false)"
  (into {} (map (fn [k] [k (filter #(not (= false (get-in % [:use-for-generation] true)))
                                   (get lexicon k))])
                (keys lexicon))))
(defn apply-unify-key [lexicon]
  (into {}
        (for [[k vals] lexicon]
          [k
           (map (fn [v]
                  (cond
                    (map? v)
                    (reduce unify
                            (cons (dissoc v :unify)
                                  (map (fn [each-unify-arg]
                                         (cond (fn? each-unify-arg)
                                               (each-unify-arg)
                                               true each-unify-arg))
                                       (:unify v))))
                    true v))
                vals)])))

