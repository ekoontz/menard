(ns babel.exception
  ^{:doc "exceptions that work either in clojure or clojurescript"}
  (:refer-clojure))

(defn exception [error-string]
  #?(:clj
     (throw (Exception. error-string)))
  #?(:cljs
     (throw (js/Error. error-string))))
