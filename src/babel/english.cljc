(ns babel.english
  (:refer-clojure :exclude [get-in])
  (:require
   [dag_unify.core :refer (fail-path get-in unifyc)]
   [babel.generate :as generate]
   [babel.english.grammar :as grammar]
   [babel.english.morphology :as morph]
   [babel.over :refer [over truncate]]
   [babel.parse :as parse]
   [clojure.repl :refer [doc]]
   [clojure.string :as string]
   #?(:cljs [babel.logjs :as log])
   #?(:clj [clojure.tools.logging :as log])
   [dag_unify.core :refer [deserialize dissoc-paths
                           fail? fail-path get-in serialize strip-refs]]))

(def medium-model (promise))
(defn medium []
  (if (and (not (nil? medium-model))
           (realized? medium-model))
    @medium-model
    @(do
       (log/info (str "creating model..(will be slow the first time).."))
       (let [result (deliver medium-model (grammar/medium))]
         (log/info (str "done."))
         result))))

(declare morph)

(defn generate
  ([]
   (let [max-total-depth generate/max-total-depth
         truncate-children true
         model (medium)]
     (generate {:modified false}
               :max-total-depth max-total-depth
               :truncate-children true
               :model model)))
   
  ([spec & {:keys [max-total-depth model truncate-children]
            :or {max-total-depth generate/max-total-depth
                 truncate-children true
                 model (medium)}}]
   (log/debug (str "generating with spec: " (strip-refs spec) " with max-total-depth: " max-total-depth))
   (let [result (generate/generate spec model
                                   :max-total-depth max-total-depth
                                   :truncate truncate-children)]
     (if (keyword? result)
       (throw (Exception. (str "please don't send me a keyword :( : this is what you sent me: " result)))
       (conj {:surface (morph result)}
             result)))))

(defn generate-all
  ([]
   (let [max-total-depth generate/max-total-depth
         truncate-children true
         model (medium)]
     (generate-all {:modified false
                    :synsem {:subcat '()}}
                   :max-total-depth max-total-depth
                   :truncate-children true
                   :model model)))
   
  ([spec & {:keys [max-total-depth model truncate-children]
            :or {max-total-depth generate/max-total-depth
                 truncate-children true
                 model (medium)}}]
   (log/debug (str "generating with spec: " (strip-refs spec) " with max-total-depth: " max-total-depth))
   (let [result (generate/generate-all spec model 0 max-total-depth)]
     (map #(conj % {:surface (morph %)})
          result))))

;; can't decide between 'morph' or 'fo' or something other better name.
(defn morph [expr & {:keys [from-language show-notes]
                     :or {from-language nil
                          show-notes false}}]
  (morph/fo expr
            :from-language from-language :show-notes show-notes
            :lexicon (:lexicon (medium))))

(defn fo-ps [expr]
  (parse/fo-ps expr fo))

(defn analyze
  ([surface-form]
   (analyze surface-form (:lexicon (medium))))
  ([surface-form lexicon] ;; use user-provided lexicon
   (morph/analyze surface-form lexicon)))

(defn preprocess [input]
  "arbitrary regexp replacements to convert English orthography into a parsable whitespace-delimited expression"
  ;; e.g.
  ;; (preprocess "the womens' hats and the cats' pyjamas")
  ;;  => "the women 's  hats and the cats 's pyjamas"
  ;;
  (string/replace (string/replace input #"(men)s'(\s*)" "$1 's $2") #"(\S)s'" "$1s 's"))

(defn parse
  "parse a string in English into zero or more (hopefully more) phrase structure trees"

  ([input]
   (parse (preprocess input) (medium)))

  ([input truncate?]
   (parse (preprocess input) (medium) truncate?))

  ([input model truncate?]
   (parse/parse (preprocess input) model truncate?)))

(def tree-variants
  [
   {:head {:phrasal true}}

   {:comp {:phrasal true}}

   {:head {:phrasal true}
    :comp {:phrasal true}}

   {:head {:phrasal true}
    :comp {:phrasal true
           :head {:phrasal true}}}
   
   {:comp {:phrasal true
           :head {:phrasal true}}
    :head {:phrasal true}}

   ;; failsafe
   {:comp {:phrasal :top
           :head :top}}])

(defn sentences [ & [count as-numbered-list spec model]]
  (let [as-numbered-list (or (nil? as-numbered-list)
                             (= as-numbered-list "true"))
        count (or (Integer. count) 100)
        model (or model (medium))
        preds (vec (set (filter #(not (= :top %))
                                (map #(get-in % [:synsem :sem :pred] :top)
                                     (filter #(= (get-in % [:synsem :cat]) :verb)
                                             (flatten (vals (:lexicon (medium)))))))))
        spec-fn (or (and spec (fn [] [spec]))
                    (fn []
                      (map (fn [variant]
                             (merge {:synsem {:sem {:pred (first (take 1 (shuffle preds)))}
                                              :cat :verb}}
                                    variant))
                           (lazy-seq (shuffle tree-variants)))))]
    (println (str "count: " count))
    (doall (map
            (fn [num]
              (let [specs (spec-fn)
                    expr (first (->> (lazy-seq specs)
                                     (map (fn [spec]
                                            (let [result (generate spec :model model)]
                                              (if (empty? (morph result :show-notes false))
                                                (do (log/info (str "failed to generate with spec: " spec))
                                                    nil)
                                                result))))
                                     (remove nil?)
                                     (take 1)))
                    fo (morph expr :show-notes false)]
                (let [to-print
                      (cond
                        (nil? expr) (str " expr: specs=" (string/join "," specs))
                        (empty? fo) (str " failed: specs=" (string/join "," specs))
                        true
                        (str (string/capitalize (nth fo 0))
                             (string/join "" (rest (vec fo)))
                             "."))]
                  (cond
                    as-numbered-list
                    (println (str (+ 1 num) "." to-print))

                    (= 0 (mod num 7))
                    (print (str "\n\t" to-print " "))

                    true
                    (print (str to-print " "))))))
            (range 0 count)))))
