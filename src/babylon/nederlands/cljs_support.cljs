(ns babylon.nederlands.cljs_support
  (:require-macros [babylon.nederlands])
  (:require [babylon.generate :as g]
            [babylon.nederlands :as nl]
            [babylon.serialization :as s]
            [cljslog.core :as log]
            [dag_unify.core :as u]))
(def lexicon-atom (atom nil))

(defn lexicon []
  (if (nil? @lexicon-atom)
    (do (swap! lexicon-atom
               (fn []
                 (-> (nl/read-compiled-lexicon)
                     babylon.lexiconfn/deserialize-lexicon              
                     vals
                     flatten)))
        @lexicon-atom)
    @lexicon-atom))

(def lexeme-map-atom (atom nil))

;; note that we exclude [:exception]s from the lexemes that we use for
;; generation since they are only to be used for parsing.
;; TODO: this is duplicated in babylon/nederlands.cljc (see def verb-lexicon).
(defn lexeme-map []
    (if (nil? @lexeme-map-atom)
      (do (swap! lexeme-map-atom
                 (fn []
                   {:verb (->> (lexicon)
                               (filter #(= :verb (u/get-in % [:cat])))
                               (filter #(not (u/get-in % [:exception]))))
                    :det (->> (lexicon)
                              (filter #(= :det (u/get-in % [:cat]))))
                    :intensifier (->> (lexicon)
                                      (filter #(= :intensifier (u/get-in % [:cat]))))
                    :noun (->> (lexicon)
                               (filter #(= :noun (u/get-in % [:cat])))
                               (filter #(not (u/get-in % [:exception]))))
                    :top (lexicon)
                    :adjective (->> (lexicon)                                                          
                                    (filter #(= :adjective (u/get-in % [:cat]))))})))
      @lexeme-map-atom))

(defn index-fn [spec]
  ;; for now a somewhat bad index function: simply returns
  ;; lexemes which match the spec's :cat, or, if the :cat isn't
  ;; defined, just return all the lexemes.
  (let [result (get (lexeme-map) (u/get-in spec [:cat] :top) nil)]
    (if (not (nil? result))
        (shuffle result)
        (do
          (log/warn (str "no entry from cat: " (u/get-in spec [:cat] ::none) " in lexeme-map: returning all lexemes."))
          (lexicon)))))

(def grammar-atom (atom nil))
(def expressions-atom (atom nil))

(defn grammar []
  (->> (nl/read-compiled-grammar)
       (map dag_unify.serialization/deserialize)))

(def expressions-atom (atom nil))

(defn expressions []
  (or @expressions-atom
      (do (swap! expressions-atom (fn [] (nl/read-expressions))))))

(defn syntax-tree [tree]
  (s/syntax-tree tree (nl/morphology)))

(defn generate [spec & [times]]
  (let [attempt
        (try
          (g/generate spec
                      (grammar)
                      (fn [spec]
                        (shuffle (index-fn spec)))
                      syntax-tree)
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
         :syntax-tree (syntax-tree attempt)
         :surface (nl/morph attempt)})))
