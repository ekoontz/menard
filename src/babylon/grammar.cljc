(ns babylon.grammar
  (:require [babylon.morphology :as m]
            [clojure.string :as string]
            [dag_unify.core :as u :refer [unify]]))

(declare default-morph-fn)
(def ^:dynamic morph default-morph-fn)
(def ^:dynamic morph-leaf m/morph-leaf)

(defn default-morph-fn [structure]
  (cond (or (= :fail structure) 
            (nil? structure)
            (string? structure)) structure

        (seq? structure)
        (map morph structure)
        
        (= false (u/get-in structure [:phrasal]))
        (morph-leaf structure)
        
        true
        (string/join " "
                     (map morph
                          [(u/get-in structure [:1] "_")
                           (u/get-in structure [:2] "_")]))))

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



