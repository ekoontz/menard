(ns menard.subcat
  (:require
   [dag_unify.core :as u :refer [unify]]
   #?(:clj [clojure.java.io :as io :refer [resource]])
   [menard.ug :as ug]
   [menard.lexiconfn :refer [read-and-eval]]
   [menard.model :refer [use-path]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])))

(declare define)

#?(:clj
   (defn load-from-file []
     (define (with-open [r (io/reader "/Users/ekoontz/menard/resources/subcat.edn")]
               (eval (read (java.io.PushbackReader. r)))))))

#?(:clj
   (defn load-from-jar []
     (-> "subcat.edn" use-path read-and-eval define)))

(defn define [defs]
  (let [result
        (->> defs
             (map (fn [def]
                    (let [k (:def def)
                          v (-> def (dissoc :unify) (dissoc :def))]
                       (let [unify-with (:unify def)]
                        (let [value (if unify-with
                                      (do
                                         (reduce unify (cons v
                                                             (map (fn [x]
                                                                    (if (re-find #"/" (str (symbol x)))
                                                                      (eval (symbol (str x)))
                                                                      (eval (symbol "menard.subcat" (str x)))))
                                                                  unify-with))))
                                      v)
                              k (symbol k)]
                          (intern 'menard.subcat k value))))))
             doall)]
    (log/info (str "loaded: " (count result) " symbols."))))

(load-from-jar)
