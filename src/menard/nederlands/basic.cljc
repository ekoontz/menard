(ns menard.nederlands.basic
  (:require [dag_unify.core :as u :refer [unify]]
            [clojure.tools.logging :as log]
            [menard.generate :as g]
            [menard.nederlands.compile :refer [compile-lexicon]]
            [menard.model :refer [create]]
            [menard.punctuation :refer [sentence-punctuation]]
            [menard.serialization :as s]))

(defn basic-filter
  "create a 'basic' lexicon that only contains all closed-class words, but
   only :basic open-class words"
  [lexicon]
  (->>
   (keys lexicon)
   (map (fn [k]
          (let [vals (get lexicon k)
                filtered-vals (->> vals
                                   (filter (fn [lexeme]
                                             (let [cat (u/get-in lexeme [:cat])
                                                   curriculum (u/get-in lexeme [:curriculum] ::none)]
                                               (or
                                                true
                                                (and (= cat :adjective)
                                                     (= :basic curriculum))
                                                (and (= cat :adverb)
                                                     (= :basic curriculum))
                                                (and (= cat :conjunction))
                                                (and (= cat :det))
                                                (and (= cat :exclamation))
                                                (and (= cat :intensifier))
                                                (and (= cat :misc))
                                                (or (and (= cat :noun)
                                                         (true? (u/get-in lexeme [:pronoun?]))))
                                                (or (and (= cat :noun)
                                                         (true? (u/get-in lexeme [:propernoun?]))))
                                                (or (and (= cat :noun)
                                                         (= :basic curriculum)))
                                                (and (= cat :numbers))
                                                (and (= cat :preposition))
                                                (and (= cat :verb)
                                                     (= :basic curriculum)))))))]
            (if (seq filtered-vals)
              {k filtered-vals}))))
   (into {})))

(def model
  (delay (create "nederlands/models/basic"
               "basic"
               compile-lexicon
               true)))

#?(:clj
   (defn syntax-tree [tree]
     (s/syntax-tree tree (:morphology @model))))

#?(:cljs
   (defn syntax-tree [tree]
     (s/syntax-tree tree [])))

(defn generate
  "generate one random expression that satisfies _spec_."
  [spec]
  (let [model @model
        name (-> model :spec :name)]
    (if name
      (log/debug (str "menard.nederlands/generate: generating with model named: " name))
      (log/warn (str "generating with model with no name, but has keys: " (keys model)
                     " and maybe a spec? " (:spec model))))

    ;; TODO: these bindings will go away soon.
    (let [retval
          (binding [g/max-depth (:max-depth spec g/max-depth)
                    g/max-fails (:max-fails spec g/max-fails)
                    g/allow-backtracking? true]
            (-> spec
                ((fn [x] (unify x (:training-wheels x :top))))
                (dissoc :training-wheels)
          (g/generate (-> model :grammar)
                      (-> model :lexicon-index-fn)
                      syntax-tree)))]
      (log/debug (str "menard.nederlands/generate: generated: " (-> retval syntax-tree)))
      retval)))

(defn morph
  ([tree]
   (cond
     (map? (u/get-in tree [:syntax-tree]))
     (s/morph (u/get-in tree [:syntax-tree]) (:morphology @model))

     :else
     (s/morph tree (:morphology @model))))

  ([tree & {:keys [sentence-punctuation?]}]
   (when sentence-punctuation?
     (-> tree
         morph
         (sentence-punctuation (u/get-in tree [:sem :mood] :decl))))))

