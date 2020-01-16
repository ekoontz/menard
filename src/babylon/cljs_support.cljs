(ns babylon.cljs_support
  (:require-macros [babylon.nederlands] ;; TODO: remove
                   [babylon.grammar])
  (:require [babylon.generate :as g]
            [babylon.grammar :as gra]
            [babylon.nederlands :as nl] ;; TODO: remove
            [babylon.serialization :as s]
            [cljslog.core :as log]
            [dag_unify.core :as u]))

(def grammar
  (->> (gra/read-compiled-grammar "babylon/nederlands/grammar/compiled.edn")
       (map dag_unify.serialization/deserialize)))

(defn expressions []
  (or @nl/expressions-atom
      (do (swap! nl/expressions-atom (fn [] (nl/read-expressions))))))

(defn generate [spec & [times]]
  (let [attempt
        (try
          (g/generate spec
                      grammar
                      (fn [spec]
                        (shuffle (index-fn spec)))
                      nl/syntax-tree)
          (catch js/Error e
            (cond
              (or (nil? times)
                  (< times 2))
              (do
                (log/warn (str "retry #" (if (nil? times) 1 (+ 1 times))))
                (generate spec (if (nil? times) 1 (+ 1 times))))
              true nil)))]
      (cond
        (and (or (nil? times)
                 (< times 2))
             (or (= :fail attempt)
                 (nil? attempt)))
        (do
          (log/info (str "retry #" (if (nil? times) 1 (+ 1 times))))
          (generate spec (if (nil? times) 1 (+ 1 times))))
        (or (nil? attempt) (= :fail attempt))
        (log/error (str "giving up generating after 2 times; sorry."))
        true
        {:structure attempt
         :syntax-tree (nl/syntax-tree attempt)
         :surface (nl/morph attempt)})))

(def lexicon
  (-> (nl/read-compiled-lexicon)
      babylon.lexiconfn/deserialize-lexicon              
      vals
      flatten))

;; note that we exclude [:exception]s from the lexemes that we use for
;; generation since they are only to be used for parsing.
;; TODO: this is duplicated in babylon/nederlands.cljc (see def verb-lexicon).
(def lexeme-map
  {:verb (->> lexicon
              (filter #(= :verb (u/get-in % [:cat])))
              (filter #(not (u/get-in % [:exception]))))
   :det (->> lexicon
             (filter #(= :det (u/get-in % [:cat]))))
   :intensifier (->> lexicon
                     (filter #(= :intensifier (u/get-in % [:cat]))))
   :noun (->> lexicon
              (filter #(= :noun (u/get-in % [:cat])))
              (filter #(not (u/get-in % [:exception]))))
   :top lexicon
   :adjective (->> lexicon
                   (filter #(= :adjective (u/get-in % [:cat]))))})

(defn index-fn [spec]
  ;; for now a somewhat bad index function: simply returns
  ;; lexemes which match the spec's :cat, or, if the :cat isn't
  ;; defined, just return all the lexemes.
  (let [result (get lexeme-map (u/get-in spec [:cat] :top) nil)]
    (if (not (nil? result))
        (shuffle result)
        (do
          (log/warn (str "no entry from cat: " (u/get-in spec [:cat] ::none) " in lexeme-map: returning all lexemes."))
          lexicon))))
