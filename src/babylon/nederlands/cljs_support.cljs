(ns babylon.nederlands.cljs_support
  (:require-macros [babylon.morphology]
                   [babylon.grammar]
                   [babylon.lexiconfn])
  (:require [babylon.generate :as g]
            [babylon.grammar :as gra]
            [babylon.lexiconfn :as l]
            [babylon.morphology :as morph]
            [babylon.nederlands :as nl]
            [babylon.serialization :as s]
            [cljslog.core :as log]
            [dag_unify.core :as u]))

(defn generate [spec & [times]]
  (let [attempt
        (try
          (g/generate spec
                      nl/grammar
                      (fn [spec]
                        (shuffle (nl/index-fn spec)))
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

