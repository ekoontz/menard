(ns babylon.generate.truncate
  (:require [dag_unify.core :as u]
            [dag_unify.dissoc :as d]
            [clojure.tools.logging :as log]))

(defn truncate-at [tree path morph-ps]
  (let [reentrances (map first (u/serialize tree))
        aliases
        (filter #(or (= (first %) :comp)
                     (= (first %) :head)
                     (= (first %) :1)
                     (= (first %) :2))
                (set (d/aliases-of path reentrances)))]
    (binding [d/remove-path?
              (fn [path]
                (some #(d/prefix? path %) aliases))]
      (log/debug (str "truncating:" (morph-ps tree) " at path: " (vec path) " size=" (count (str tree))))
      (let [truncated
            (->
             tree
             (d/dissoc-in (concat path [:head]))
             (d/dissoc-in (concat path [:comp]))
             (u/assoc-in! (concat path [:morph-ps]) (morph-ps (u/get-in tree path)))
             (u/assoc-in! (concat path [::done?]) true))]
        (log/debug (str "     to:   " (morph-ps truncated) "; size=" (count (str truncated))))
        truncated))))

(defn truncate-up [tree frontier-path morph-ps truncate?]
  (log/debug (str "truncat-up:" (morph-ps tree) " at: " (vec frontier-path)))
  (cond (not truncate?)
        tree

        (empty? frontier-path)
        tree
        
        (and (true? (u/get-in tree (concat frontier-path [::done?])))
             (empty? (butlast frontier-path)))
        (do
          (log/debug (str "truncating tree: " (morph-ps tree) " at: " (vec frontier-path)))
                          
          (->
           tree
           (truncate-at frontier-path morph-ps)
           (u/assoc-in! (concat frontier-path [:morph-ps]) (morph-ps (u/get-in tree frontier-path)))
           (u/assoc-in! (concat frontier-path [::done?]) true)))
        
        (u/get-in tree (concat (butlast frontier-path) [:phrasal]))
        (->
         tree
         (truncate-up (butlast frontier-path) morph-ps))

        (and (u/get-in tree (concat frontier-path [:phrasal]))
             (u/get-in tree (concat frontier-path [::done?])))
        (truncate-at tree frontier-path morph-ps)

        true
        (do
          (log/debug (str "not truncating any more with path:" (vec frontier-path)))
          tree)))

