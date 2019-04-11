(ns babylon.grammar
  (:require [babylon.morphology :as m]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [dag_unify.core :as u :refer [unify]]))

(defn list-as-map-to-list
  "turn a map represention of a list: e.g. {:first :a :rest {:first :b}}
   into a list: e.g. [a b]"
  [m]
  (if (u/get-in m [:first])
     (cons (u/get-in m [:first])
           (list-as-map-to-list (u/get-in m [:rest])))))

(defn default-morph-fn [structure morphology]
  (cond (or (= :fail structure) 
            (nil? structure)
            (string? structure)) structure

        (seq? structure)
        (map (fn [structure]
               (default-morph-fn structure morphology))
             structure)

        (u/get-in structure [:surface])
        (:surface structure)
        
        (u/get-in structure [:words])
        (let [words (list-as-map-to-list (u/get-in structure [:words]))]
          (string/join " " (map (fn [word] (default-morph-fn word morphology))
                                words)))
        
        (= false (u/get-in structure [:phrasal] false))
        (m/morph-leaf structure morphology)
        
        (and (u/get-in structure [:1])
             (u/get-in structure [:2]))
        (string/join " "
                     (map (fn [structure]
                            (default-morph-fn structure morphology))
                          [(u/get-in structure [:1] "_")
                           (u/get-in structure [:2] "_")]))
        true "_"))

(defn syntax-tree [structure morphology]
    (cond (or (= :fail structure) 
              (nil? structure)
              (string? structure)) structure

          (seq? structure)
          (map (fn [structure]
                 (syntax-tree structure morphology))
               structure)

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
              (map (fn [structure] (syntax-tree structure morphology))
                   ["[" (:rule structure)
                    (if (get structure :babel.generate/done?)
                      "+" " ")
                    " "
                    one (u/get-in structure [:1] "-") " "
                    two (u/get-in structure [:2] "-")
                    "]"])))

          (map? structure)
          (default-morph-fn structure morphology)))

(defn process [grammar]
  (->> grammar

       ;; each member of :unify in a rule is a symbol.
       ;; evaluate each symbol, which should be a dag, and
       ;; combine all of them with the original rule.
       (map #(reduce unify
                     (cons (dissoc % :unify)
                           (map eval (:unify %)))))

       ;; for each member of :option in a rule,
       ;; create a new rule unified with that member.
       (mapcat (fn [base-rule]
                 (->> (eval (:options base-rule [:top]))
                      (map (fn [option]
                             (unify base-rule option)))
                      (filter #(not (= % :fail))))))))
