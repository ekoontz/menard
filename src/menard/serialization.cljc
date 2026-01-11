(ns menard.serialization
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [menard.log :as log])
   [menard.lexiconfn :as l]
   [menard.morphology :as m]
   [dag_unify.core :as u]))

(defn morph [tree morphology]
  (cond
    (nil? tree) "_"
    (string? tree) tree    
    (string? (u/get-in tree [:surface]))
    (u/get-in tree [:surface])
    (u/get-in tree [:1])
    (str (morph (u/get-in tree [:1]) morphology) " "
         (morph (u/get-in tree [:2]) morphology))
    (u/get-in tree [:2])
    (str "_ "
         (morph (u/get-in tree [:2]) morphology))
    :else
    (do
      (let [retval (m/morph-leaf tree morphology)]
        (log/debug (str "called m/morph-leaf with tree: " (l/pprint tree) "; retval: " retval))
        retval))))

(defn syntax-tree [tree morphology]
  (cond
    (nil? tree) "_"
    (string? tree) tree    
    (string? (u/get-in tree [:syntax-tree]))
    (do
      (log/info (str "syntax-tree from string: " (u/get-in tree [:syntax-tree])))
      (u/get-in tree [:syntax-tree]))
    (u/get-in tree [:1])
    (str "["
         (:rule tree "?")
         (when (:variant tree) (str "(" (:variant tree) ")" ""))
         (when (let [defined? (u/get-in tree [:reflexive?])]
                 (and (not (= defined? ::none))
                      (not (= defined? :top))))
           (let [value (u/get-in tree [:reflexive?])]
             (if value
               (str "{" (cond (= value true)
                              "+"
                              (= value false)
                              "-"
                              (nil? value) ""
                              :else value) "}"))))
           " "
         (if (or (= true (u/get-in tree [:1 :head?]))
                 (= (u/get-in tree [:1]) (u/get-in tree [:head])))
           "+" ".")
         (syntax-tree (u/get-in tree [:1]) morphology) " "
         (if (or (= true (u/get-in tree [:2 :head?]))
                 (= (u/get-in tree [:2]) (u/get-in tree [:head])))
           "+" ".")
         (syntax-tree (u/get-in tree [:2]) morphology)
         "]")
    (u/get-in tree [:2])
    (str "["
         (:rule tree "?") " "
         (if (= true (u/get-in tree [:1 :head?]))
           "+" ".")
         "_ "
         (if (= true (u/get-in tree [:2 :head?]))
           "+" ".")
         (syntax-tree (u/get-in tree [:2]) morphology) "]")
    :else
    (m/morph-leaf tree morphology {:show-sense? true})))

(defn get-derivation [structure]
  (let [d (u/get-in structure [:menard.lexiconfn/derivation])
        hd (u/get-in structure [:head-derivation])
        hd (if (keyword? hd) nil hd)
        cd (u/get-in structure [:comp-derivation])
        cd (if (keyword? cd) nil cd)
        encoded-d (if (seq d)
                    (menard.lexiconfn/encode-derivation d))
        encoded-hd (if (seq hd)
                     (menard.lexiconfn/encode-derivation hd))
        encoded-cd (if (seq cd)
                     (menard.lexiconfn/encode-derivation cd))]
    (cond (seq encoded-d)
          encoded-d
          (seq encoded-hd)
          encoded-hd
          (seq encoded-cd)
          encoded-cd)))

(defn pprint [structure]
  (let [d (u/get-in structure [:menard.lexiconfn/derivation])
        hd (u/get-in structure [:head-derivation])
        cd (u/get-in structure [:comp-derivation])
        encoded-d (if (seq d)
                    (menard.lexiconfn/encode-derivation d))
        encoded-hd (if (seq hd)
                     (menard.lexiconfn/encode-derivation hd))
        encoded-cd (if (seq cd)
                     (menard.lexiconfn/encode-derivation cd))]
    (-> structure
        (dissoc :menard.lexiconfn/derivation)
        (dissoc :head-derivation)
        (dissoc :comp-derivation)
        ((fn [structure]
           (if (seq encoded-d)
             (assoc structure :derivation encoded-d)
             structure)))
        ((fn [structure]
           (if (seq encoded-hd)
             (assoc structure :head-derivation encoded-hd)
             structure)))
        ((fn [structure]
           (if (seq encoded-cd)
             (assoc structure :comp-derivation encoded-cd)
             structure)))
        u/pprint)))

(defn determined? [tree path syntax-tree-fn]
  (log/info (str "determined? for tree: " (syntax-tree-fn tree) " and path: " (vec path)))
  (log/info (str "agr for tree: " (syntax-tree-fn tree) " and path: " (vec path) ": "
                 (u/get-in tree (concat path [:agr]))))
  (let [phrasal? (u/get-in tree [:phrasal?])
        head-phrasal? (u/get-in tree [:head :phrasal?])
        comp-phrasal? (u/get-in tree [:comp :phrasal?])]
    (log/info (str "phrasal? " phrasal?))
    (log/info (str "  head-phrasal? " head-phrasal?))
    (log/info (str "  comp-phrasal? " comp-phrasal?))    
    false))




