(ns menard.test.español.adjectives
  (:require [menard.español :as es
             :refer [generate morph parse syntax-tree]]
            [menard.lexiconfn :as l]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u :refer [unify]]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

;; if you made changes to these, you can uncomment them to reload them
;; so that in turn the below model will be reloaded with these changes:
;;(load "../../../../src/menard/subcat")
;;(load "../../../../src/menard/español/tenses")

;; reload the model every time to help with debugging the model:
(load "../../../../src/menard/español")

(def model es/model)

(deftest agreement
  (is (= (->> "el gato holgazán" parse (map syntax-tree) first)
         "[np .el +[nbar +gato .holgazán]]"))
  (is (= (->> "los gatos holgazán" parse (map syntax-tree) first)
         "[np .los +[nbar +gatos .holgazán]]"))
  (is (= "[np .la +[nbar +mesa .holgazana]]"
         (->> "la mesa holgazana" parse (map syntax-tree) first)))
  (is (= "[np .las +[nbar +mesas .holgazanes]]"
         (->> "las mesas holgazanes" parse (map syntax-tree) first)))
  (is (= "[np .los +[nbar +gatos .malos]]"
         (->> "los gatos malos" parse (map syntax-tree) first))))

