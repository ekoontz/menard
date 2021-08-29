(ns menard.grammar
  (:require [menard.exception :refer [exception]]
            #?(:clj [clojure.java.io :as io :refer [resource]])
            [menard.morphology :as m]
            [clojure.string :as string]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.serialization :as s :refer [serialize]]))

(defn list-as-map-to-list
  "turn a map represention of a list: e.g. {:first :a :rest {:first :b}}
   into a list: e.g. [a b]"
  [m]
  (when (u/get-in m [:first])
    (cons (u/get-in m [:first])
          (list-as-map-to-list (u/get-in m [:rest])))))

(defn process [grammar & [do-not-eval?]]
  (->> grammar

       ;; each member of :unify in a rule is a symbol.
       ;; evaluate each symbol, which should be a dag, and
       ;; combine all of them with the original rule:
       (map #(reduce unify
                     (cons (dissoc % :unify)
                           (map (fn [each]
                                  (if do-not-eval? each (eval each)))
                                (:unify %)))))

       ;; for each member of :option in a rule,
       ;; create a new rule unified with that member.
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
                   result)))

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
                         (exception error-message)))))

       (filter (fn [input-rule]
                 (cond (and (keyword? (u/get-in input-rule [:cat]))
                            (not (= :top (u/get-in input-rule [:cat]))))
                       (do (log/debug (str "rule: " (u/get-in input-rule [:rule]) " is ok: :cat is specified to: " (u/get-in input-rule [:cat])))
                           true)

                       :else
                       (let [warn-message (str "rule: " (u/get-in input-rule [:rule]) " has no :cat value specified: might overgeneralize unexpectedly.")]
                         (log/warn warn-message)
                         true))))

       (map #(u/assoc-in % [:phrasal] true))
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
