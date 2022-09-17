(ns menard.ug
  (:require
   [dag_unify.core :as u :refer [unify]]
   #?(:clj [clojure.java.io :as io :refer [resource]])
   [menard.lexiconfn :refer [read-and-eval]]
   #?(:clj [menard.path :refer [use-path]])
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])))

(declare define)

#?(:clj
   (defn load-from-file []
     (define (with-open [r (io/reader "/Users/ekoontz/menard/resources/ug.edn")]
               (eval (read (java.io.PushbackReader. r)))))))

#?(:clj
   (defn load-from-jar []
     (-> "ug.edn" use-path read-and-eval define)))

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
                                                                    (eval (symbol "menard.ug" (str x))))
                                                                  unify-with))))
                                      v)
                              k (symbol k)]
                          (intern 'menard.ug k value))))))
             doall)]
    (log/info (str "loaded: " (count result) " symbols."))))

(load-from-jar)



