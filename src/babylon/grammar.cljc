(ns babylon.grammar
  (:require [babylon.morphology :as m]
            [clojure.string :as string]
            [dag_unify.core :as u :refer [unify]]))

(declare morph)
(declare morph-leaf)

(defn default-morph-fn [structure]
  (cond (or (= :fail structure) 
            (nil? structure)
            (string? structure)) structure

        (seq? structure)
        (map morph structure)
        
        (= false (u/get-in structure [:phrasal]))
        (morph-leaf structure)
        
        (and (u/get-in structure [:1])
             (u/get-in structure [:2]))
        (string/join " "
                     (map morph
                          [(u/get-in structure [:1] "_")
                           (u/get-in structure [:2] "_")]))
        true "_"))

(def ^:dynamic morph default-morph-fn)

;; TODO: remove this usage of ^:dynamic; this is difficult, because if we
;; change morphology.cljc, we have to load this file (grammar.cljc)
;; for the morphology.cljc changes to become effective.
(def ^:dynamic morph-leaf m/morph-leaf)

(defn syntax-tree [structure]
    (cond (or (= :fail structure) 
              (nil? structure)
              (string? structure)) structure

          (seq? structure)
          (map syntax-tree structure)

          (u/get-in structure [:syntax-tree])
          (:syntax-tree structure)
          
          (or (:head structure)
              (:comp structure))
          (let [one (cond (= (get structure :1)
                             (get structure :head))
                          "*"
                          (= (get structure :1)
                             (get structure :comp))
                          "."

                          (= (get structure :2)
                             (get structure :head))
                          "."
                          (= (get structure :2)
                             (get structure :comp))
                          "*"

                          true
                          (throw (Exception. (str "the :1 is neither :head nor :comp: "
                                                  (type structure) "; empty: " (empty? structure) "; keys: "
                                                  (keys structure) "; "
                                                  (vec (:dag_unify.core/serialized structure))))))

                two (cond (= (get structure :2)
                             (get structure :head))
                          "*"
                          (= (get structure :2)
                             (get structure :comp))
                          "."
                          (= (get structure :1)
                             (get structure :head))
                          "."
                          (= (get structure :1)
                             (get structure :comp))
                          "*"
                          
                          true
                          (throw (Exception. (str "the :2 is neither :head nor :comp: "
                                                  (vec (:dag_unify.core/serialized structure))))))]

            (string/join ""
              (map syntax-tree
                   ["[" (:rule structure)
                    (if (get structure :babel.generate/done?)
                      "+" " ")
                    " "
                    one (u/get-in structure [:1] "-") " "
                    two (u/get-in structure [:2] "-")
                    "]"])))

          (map? structure)
          (morph structure)))

(defn process [grammar]
  (->> grammar

       ;; each member of :unify in a rule is a symbol.
       ;; evaluate each symbol, which should be a dag, and reduce-unify the
       ;; rule and all such evaluated symbols in :unify.
       (map #(apply unify
                    (cons (dissoc % :unify)
                          (map eval (:unify %)))))

       ;; for each member of :option in a rule,
       ;; create a new rule unified with that member.
       (mapcat (fn [base-rule]
                 (->> (:options base-rule [:top])
                      (map (fn [option]
                             (unify base-rule option)))
                      (filter #(not (= % :fail))))))))
