(ns menard.parse
  (:require
   [clojure.set :refer [union]]
   [clojure.string :as string]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])
   [dag_unify.core :as u]
   [dag_unify.diagnostics :as diag]
   [menard.reflexives :refer [reflexive-options]]))

(def parse-only-one? false)

(def ^:dynamic lookup-fn
  (fn [_]
    []))
(def ^:dynamic grammar nil)
(def ^:dynamic syntax-tree (fn [x] (log/warn (str "'syntax-tree' was not bound."))))
(def ^:dynamic morph (fn [x] (log/warn (str "'morph' was not bound."))))
(def ^:dynamic truncate? false)
(def ^:dynamic split-on #"[ ']")
(def ^:dynamic take-this-many 30)
(def ^:dynamic debug-rule-for-comp nil)

(defn pmap-if-available [fn args]
  #?(:clj
     (pmap fn args))
  #?(:cljs
     (map fn args)))

(def log-these-rules #{"vp-modal-te" "vp-te"})

(defn overh
  "add given head as the head child of the phrase: parent."
  ;; TODO: get rid of all this type-checking in this (cond)
  ;; and use clojure.spec.
  [parent head]
  (cond
    (or (seq? head)
        (vector? head))
    (->>
     head
     (mapcat
      (fn [child]
        (overh parent child))))

    :else
    ;; TODO: 'true' here assumes that both parent and head are maps: make this assumption explicit,
    ;; and save :else for errors.
    (do
      (log/debug (str "overh: parent: " (syntax-tree parent)))
      (log/debug (str "overh: head:   " (syntax-tree head)))
      (let [pre-check? (not (= :fail
                               (u/get-in parent [:head :cat] :top)
                               (u/get-in head [:cat] :top)))
            result (cond pre-check?
                         (u/unify parent
                                  {:head head})
                         :else
                         (do
                           (log/debug (str "failed precheck: parent: " (syntax-tree
 parent) "; head: " (syntax-tree head) "; "
                                           "parent [:head :cat]=" (u/get-in parent [:head :cat]) "; head [:cat]=" (u/get-in head [:cat])))
                           :fail))]
        (if (not (= :fail result))
          (do
            (log/debug (str "overh success: " (syntax-tree parent) " -> " (syntax-tree result)))
            [result])
          (do
            (when (contains? log-these-rules (u/get-in parent [:rule]))
              (log/debug
               (str "overh fail:   " (syntax-tree parent) " <- " (syntax-tree head)
                    " " (let [fp (diag/fail-path parent {:head head})]
                          {:path (:path fp)
                           :arg1 (u/pprint (:arg1 fp))
                           :arg2 (u/pprint (:arg2 fp))}))))))))))
  
(defn overc
  "add given child as the complement of the parent"
  [parent comp]
  (cond
    (or (seq? parent)
        (vector? parent))
    (let [parents parent]
      (mapcat (fn [parent]
                (overc parent comp))
              parents))
    
    (or (seq? comp)
        (vector? comp))
    (let [comp-children comp]
      (mapcat (fn [child]
                (overc parent child))
              comp-children))
    :else
    (let [pre-check? (= (u/get-in parent [:comp :cat])
                        (u/get-in comp [:cat] (u/get-in parent [:comp :cat])))
          result
          (cond pre-check?
                (u/unify! (u/copy parent)
                          {:comp (u/copy comp)})
                :else :fail)]
      (if (not (= :fail result))
        (do
          (log/debug (str "overc success: " (syntax-tree result) " -> " (syntax-tree result)))
          [result])
        (do
          (when (contains? log-these-rules (u/get-in parent [:rule])))
          (log/debug
           (str "overc fail: " (syntax-tree parent) " <- " (syntax-tree comp)))
          (log/debug (str " "
                         (let [fp (diag/fail-path (u/copy parent)
                                                  {:comp (u/copy comp)})]
                           
                           (str " at: " (vec (:path fp)) ", parent has: " (:arg1 fp) " but comp has: " (:arg2 fp)))))
          [])))))

(declare truncate)

(defn over [parents child1 child2]
  (->>
   parents
   (pmap-if-available
    (fn [parent]
      (let [[head comp] (if (= (:1 parent) (:head parent))
                          [child1 child2]
                          [child2 child1])]
        (-> parent
            (overh head)
            (overc comp)))))
   (reduce
    (fn [a b]
      (lazy-cat a b)))

   (map (fn [expression]
          (map (fn [option]
                 (u/unify option expression))
               (shuffle reflexive-options))))

   (flatten)
   
   (remove #(= % :fail))

   (map (fn [tree]
          (if truncate?
            (truncate tree syntax-tree)
            tree)))))


(defn span-pairs [i n]
  (cond (= i 0)
        (->> (range 0 (- n 1))
             (map (fn [x] [[0 (+ 1 x)][(+ 1 x) n]])))
        true
        (concat
         (span-pairs (- i 1) n)
         (->> (range i (+ i (- n 1)))
              (map (fn [x] [[i (+ 1 x)][(+ 1 x) (+ i n)]]))))))

(defn summary
  "for diagnostic logging"
  [m]
  (into {}
        (->> (keys m)
             (mapcat (fn [k]
                       (if (seq (get m k))
                         [{k (map syntax-tree (get m k))}]))))))

(defn parses
  "returns a list of elements
      where each element is: [[_left_ _right_] _parses_] and 
      _parses_ is a list of structures
      where each structure is a parse covering the interval of 
      length _span-length_ in the input from _left_ to _right_."
  [input span-length]
  (cond
    (= span-length 1) input
    :else
    (let [minus-1 (-> (parses input (- span-length 1)))]
      (into minus-1
            (->>

             ;; find all span pairs of length _span-length_, where
             ;; each pair looks like [[left middle],[middle right]], so that:
             ;; 1. _middle_ is a token between the leftmost token _left_
             ;;    and the rightmost token _right_, and
             ;; 2. tokens _left_ and _right_ are _span-length_ tokens apart from each other.
             (span-pairs (- (count (keys input)) span-length) span-length)

             ;; find all parses for this span-pair [[left middle][middle right]]:
             ;; associate the span [a c] with all parses from [left right], where _middle_ is each
             ;; of the tokens between token _left_ and token _right_.
             (pmap-if-available
              (fn [[[left middle][middle right]]]
                (let [all-results (over grammar
                                        (get minus-1 [left middle])
                                        (get minus-1 [middle right]))
                      taken-results (take take-this-many all-results)
                      taken-plus-one-results (take (+ 1 take-this-many) all-results)]
                  (when (> (count taken-plus-one-results) (count taken-results))
                    (log/warn (str "more than " take-this-many " parses for: '"
                                   (morph (first taken-results)) "' ; first: "
                                   (syntax-tree (first taken-results)))))
                  
                  ;; create a new key/value pair: [left,right] => parses,
                  ;; where each parse in parses matches the tokens from [left,right] in the input.
                  (if (seq taken-results)
                    ;; <key>       <value: parses for the span of the tokens from _left_ to _right_>
                    {[left right]  taken-results})))))))))

(defn truncate [tree syntax-tree]
  (-> tree
      (assoc :syntax-tree (syntax-tree tree))
      (assoc :surface (morph tree))
      (dissoc :head)
      (dissoc :comp)
      (dissoc :1)
      (dissoc :2)))

(defn lookup-fn-with-trim [string]
  (let [trimmed (clojure.string/trim string)]
    (when (and (not (string/blank? trimmed))
               (= trimmed string))
      (lookup-fn string))))

(defn parse-tokens
  "Return a list of all possible parse trees for a list of tokens."
  [tokens]
  (let [token-count (count tokens)
        input-map (into {}
                        (map (fn [i]
                               [[i (+ i 1)]
                                (lookup-fn-with-trim (nth tokens i))])
                             (range 0 token-count)))]
      (log/debug (str "input map:" input-map))
      (let [all-parses
            (parses input-map token-count)
            result
            {:token-count token-count
             :complete-parses
             (filter map? (get all-parses
                               [0 token-count]))
             :all-parses all-parses}]
        result)))

(defn tokenize [input]
  (filter #(not (string/blank? %)) (string/split input split-on)))
  
(defn parse
  "Return a list of all possible parse trees for a string.
   Use a language-independent tokenizer (split on space and
  apostrophe) to turn the string into a sequence of tokens."
  ;; TODO: remove 'morph' as an input parameter; use a dynamic binding instead.
  [input]
  (log/debug (str "parsing input: '" input "' with syntax-tree: " syntax-tree))
  ;; tokenize input (more than one tokenization is possible), and parse each tokenization.
  (let [tokenizations (tokenize input)
        result (parse-tokens tokenizations)]
    (if (empty? (:complete-parses result))

      ;; if there are no complete parses,
      ;; cobble together results by combining
      ;; partial parses with lexical lookups of tokens (if they exists).
      ;; e.g. we can parse "er zijn katten" so there is a complete parse
      ;; but "er zijn kat" can't be fully parsed, so we return:
      ;; [er zijn] [kat].
      (let [analyses
            (zipmap
             tokenizations
             (pmap-if-available
              (fn [token]
                (lookup-fn-with-trim token))
              tokenizations))
            partial-parses (->> (vals (:all-parses result))
                                (pmap-if-available (fn [x] (->> x (filter map?))))
                                (filter seq?))]
        (log/debug (str "could not parse: \"" input "\". token:sense pairs: "
                       (string/join ";"
                                    (pmap-if-available (fn [token]
                                           (str token ":" (count (get analyses token)) ""))
                                         tokenizations))
                       (str "; partial parses: " (count (mapcat (fn [parses-for-span]
                                                                  (pmap-if-available syntax-tree parses-for-span))
                                                                partial-parses)) ".")))
        (->> (flatten partial-parses)
             (map (fn [partial-parse]
                    (merge partial-parse {::partial? true})))))
      (do (log/debug (str "parsed input:    \"" input "\""))
          (:complete-parses result)))))

