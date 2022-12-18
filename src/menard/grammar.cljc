(ns menard.grammar
  (:require [menard.exception :refer [exception]]
            #?(:clj [clojure.java.io :as io :refer [resource]])
            [menard.morphology :as m]
            [clojure.string :as string]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.diagnostics :refer [fail-path]]
            [dag_unify.serialization :as s :refer [serialize]]))

(defn list-as-map-to-list
  "turn a map represention of a list: e.g. {:first :a :rest {:first :b}}
   into a list: e.g. [a b]"
  [m]
  (when (u/get-in m [:first])
    (cons (u/get-in m [:first])
          (list-as-map-to-list (u/get-in m [:rest])))))

(defn process-options [input-grammar]
  (log/info (str "process-options: input-grammar: " (count input-grammar) " rules."))
  (let [output-grammar
        (->> input-grammar
             (mapcat (fn [base-rule]
                       (let [result
                             (->> (eval (:options base-rule [:top]))
                                  (map (fn [option]
                                         (unify base-rule option)))
                                  (filter #(not (= % :fail)))                            
                                  (map (fn [each]
                                         (-> each
                                       (dissoc :options)
                                       (dissoc :dag_unify.serialization/serialized)))))]
                         result))))]
    (log/info (str "process-options: output-grammar: " (count output-grammar) " rules."))
    output-grammar))

(defn filter-rules-by-firstness [input-grammar]
  (log/info (str "filter-rules: input-grammar: " (count input-grammar) " rules."))
  (let [output-grammar
        (->> input-grammar
             (filter (fn [input-rule]
                       (cond (= (get input-rule :head)
                                (get input-rule :1))
                             (do (log/debug (str "rule is ok: head is first: " (u/get-in input-rule [:rule])))
                                 true)
                             (= (get input-rule :head)
                                (get input-rule :2))
                             (do (log/debug (str "rule is ok: head is last: " (u/get-in input-rule [:rule])))
                                 true)
                             :else
                             (let [error-message (str "rule: " (u/get-in input-rule [:rule]) ": does not specify if the head is first or last.")]
                               (log/error error-message)
                               (exception error-message))))))]
    (log/info (str "filter-rules-by-firstness: output-grammar: " (count output-grammar) " rules."))
    output-grammar))

(defn warn-rules-by-catness [input-grammar]
  (log/info (str "warn-rules-by-catness: input-grammar: " (count input-grammar) " rules."))
  (let [output-grammar
        (->> input-grammar
             (filter (fn [input-rule]
                       (cond (and (keyword? (u/get-in input-rule [:cat]))
                                  (not (= :top (u/get-in input-rule [:cat]))))
                             (do (log/debug (str "rule: " (u/get-in input-rule [:rule]) " is ok: :cat is specified to: " (u/get-in input-rule [:cat])))
                                 true)

                             :else
                             (let [warn-message (str "rule: " (u/get-in input-rule [:rule]) " has no :cat value specified: might overgeneralize unexpectedly.")]
                               (log/warn warn-message)
                               true)))))]
    (log/info (str "warn-rules-by-catness: output-grammar: " (count output-grammar) " rules."))
    output-grammar))

(defn apply-processing-rules-to [rule processing-rules]
  (log/debug (str "looking at rule: " (:rule rule) " and applying this many processing-rules: " (count processing-rules)))
  (->> processing-rules
       (mapcat (fn [processing-rule]
                 (let [{processing-rule-name :rule
                        antecedent :if
                        consequents :then} processing-rule
                       result-if (unify rule antecedent)]
                   (if (not (= :fail result-if))
                     (->>
                      consequents
                      (map (fn [consequent]
                             (unify rule consequent)))
                      (remove #(= :fail %)))
                     [rule]))))))

(defn process [grammar & [grammar-processing-rules]]
  (log/info (str "grammar/process: processing " (count grammar) " rule" (if (not (= (count grammar) 1)) "s") "."))
  (->> grammar

       ;; each member of :unify in a rule is a symbol.
       ;; evaluate each symbol, which should be a dag, and
       ;; combine all of them with the original rule:
       (map #(reduce unify
                     (cons (dissoc % :unify)
                           (map (fn [each]
                                  (eval each))
                                (:unify %)))))
       process-options
       filter-rules-by-firstness
       warn-rules-by-catness
       
       (mapcat #(if (seq grammar-processing-rules)
                  ;; some processing rules: apply each to this grammar rule:
                  (apply-processing-rules-to % grammar-processing-rules)

                  ;; no processing rules: just return the gramamr rule:
                  [%]))
       
       (remove #(= :fail %))
       (map #(u/assoc-in % [:phrasal?] true))
       (map #(u/assoc-in % [:menard.generate/started?] true))))

#?(:clj
   (defn write-compiled-grammar [grammar write-to-file]
     (spit write-to-file (vec (->> grammar (map serialize) (map vec))))))

(defmacro read-compiled-grammar [filename]
  `~(-> filename
        resource
        slurp
        read-string))

(defmacro read-expressions [filename]
  `~(-> filename
        resource
        slurp
        read-string))

(defmacro read-grammar [filename]
  `~(-> filename
        resource
        slurp
        read-string))

(defn read-grammar-fn [filename]
  (-> filename
      ((fn [filename]
         (if (re-find #"^file:///" filename)
           (do
             (log/debug (str "read-grammar-fn: reading a file:/// filename:" filename))
             filename)
           (do
             (log/debug (str "read-grammar-fn: reading a non-file:/// filename:" filename))
             (resource filename)))))
      slurp
      read-string))
