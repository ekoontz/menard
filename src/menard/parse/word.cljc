(ns menard.parse.word
  (:require
   [clojure.string :as string]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [menard.log :as log])
   [dag_unify.core :as u]))

(defn get-possible-words [token-vector lookup-fn max-word-length-in-tokens] 
  (->> (range 0 (+ 1 (count token-vector)))
       (mapcat (fn [left]
                 (->> (range (+ 1 left) (min (+ (+ 1 max-word-length-in-tokens) left) (+ 1 (count token-vector))))
                      (map (fn [right]
                             (let [possible-word
                                   (clojure.string/join " "
                                                        (subvec token-vector left right))]
                               (when (seq (lookup-fn possible-word))
                                 (log/debug (str " found word: " possible-word))
                                 [[left right]
                                  {:surface possible-word}])))))))))

(defn get-to- [path val]
  (if (seq (rest path))
    {(first path) (get-to- (rest path) val)}
    {(first path) val}))

(defn basic [token-vector]
  (get-to- token-vector :top))

(defn get-to [paths]
  (let [ref (atom :top)]
    (map (fn [path]
           (get-to- path ref))
         paths)))

(defn get-paths-at
  "find all paths at position _i_"
  [wm i]
  (->> (keys wm)
       (filter #(= (last %) i))
       (map (fn [k]
              (log/debug (str "get-paths-at with k: " k "; i: " i))
              (concat (first (get-paths-at wm (first k)))
                      [(:surface (get wm k))])))))

(defn add-subgraph [input i wm]
  (let [paths (get-paths-at wm i)]
    (log/debug (str "add-subgraph: input: " input "; i: " i "; paths: " (vec paths)))
    (if (seq paths)
      (let [retval
            (reduce u/unify!
                    (cons input
                          (get-to paths)))]
        (log/debug (str "retval: " (u/pprint retval)))
        retval)
      input)))

(defn graph- [input wm i token-vector-count]
  (log/debug (str "graph- with input: " (u/pprint input) "; wm: " wm "; i: " i))
  (if (< i (+ 1 token-vector-count))
    (-> input
        (add-subgraph i wm)
        (graph- wm (+ 1 i) token-vector-count))
    input))

(defn word-map [token-vector lookup-fn max-word-length-in-tokens]
  (->> (get-possible-words token-vector lookup-fn max-word-length-in-tokens)
       (remove nil?)
       (into {})))

(defn graph
  "Given a string, tokenize it into a vector of tokens,
  and then use that to return a DAG that describes the possible
  paths through the vector where each arc in the graph is a word."
  [input-string split-on lookup-fn max-word-length-in-tokens]
  (let [token-vector (-> input-string
                         (clojure.string/split split-on))
        word-map (word-map token-vector lookup-fn max-word-length-in-tokens)]
    (log/debug (str "word-map: " word-map))
    (-> (basic token-vector)
        (graph- word-map 0 (count token-vector)))))

(defn groupings
  "Given a string, return all possible word groupings."
  [input-string split-on lookup-fn max-word-length-in-tokens]
  (let [graph (graph input-string split-on lookup-fn max-word-length-in-tokens)]
    (log/debug (str "groupings graph: " (u/pprint graph)))
    (u/paths graph)))

