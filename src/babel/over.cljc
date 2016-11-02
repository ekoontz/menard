(ns babel.over
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.exception :refer [exception]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [dag_unify.core :refer [copy get-in unify unify!
                           ;; temporary: until we move (truncate) from here to dag_unify, we
                           ;; need these three:
                           deserialize dissoc-paths serialize
                           ;; needed for log/debug statements:
                           strip-refs]]))

;; use map or pmap.
(def ^:const mapfn pmap)

(declare overc)
(declare overh)
(declare overhc)

;; TODO: distinguish between when:
;; 1) called with only a child1 (no child2),
;; 2) called with both a child1 and a child2, but child2's supplied value is nil:
;;    should be treated the same as empty list.
(defn over [parents child1 & [child2]]
  (cond (map? parents)
        (over (list parents) child1 child2)

        true
        (mapcat
         (fn [parent]
           (let [[head comp] (if (= (:first parent) :head)
                               [child1 child2]
                               [child2 child1])]
             (overhc parent head comp)))
         parents)))

(defn overhc [parent head comp]
  (-> parent
      (overh head)
      (overc comp)))

(defn overh
  "add given head as the head child of the phrase: parent."
  [parent head]
  ;; TODO: get rid of all this type-checking and use
  ;; whatever people use for Clojure argument type-checking.
  (cond
    (or (seq? head)
        (vector? head))
    (mapcat (fn [child]
              (overh parent child))
            head)
    true
    ;; TODO: 'true' here assumes that both parent and head are maps: make this assumption explicit,
    ;; and save 'true' for errors.
    (let [result (unify! (copy parent)
                         {:head (copy head)})]
      (if (not (= :fail result))
        (do
          (log/debug (str "overh: " (get-in parent [:rule]) " -> " (strip-refs head)))
          (list result))
        (log/debug (str "fail-path for rule: " (:rule parent) ":"
                        (dag_unify.core/fail-path
                         (copy parent)
                         {:head (copy head)})))))))

(defn overc [parent comp]
  "add given child as the complement of the parent"
  (cond
   (or (seq? parent)
       (vector? parent))
   (let [parents (lazy-seq parent)]
     (mapcat (fn [parent]
               (overc parent comp))
             parents))

   (or (seq? comp)
       (vector? comp))
   (let [comp-children comp]
     (mapcat (fn [child]
               (overc parent child))
             comp-children))
   true
   (let [result (unify! (copy parent)
                        {:comp (copy comp)})
         is-fail? (= :fail result)]
     (if (not is-fail?)
       (do
         (log/debug (str "overc: " (get-in parent [:rule]) " -> " (get-in comp [:rule]
                                                                          (get-in comp [:synsem :sem :pred]
                                                                                  "(no pred for comp)"))))
         (list result))))))

(declare show-bolt)
(declare subpath?)

(defn truncate [input truncate-paths language-model]
  (log/debug (str "truncating@" truncate-paths ":" (show-bolt input language-model)))
  (let [serialized (if (:dag_unify.core/serialized input)
                     (:dag_unify.core/serialized input)
                     (serialize input))
        paths-and-vals (rest serialized)
        path-sets (mapfn first paths-and-vals)
        path-vals (mapfn second paths-and-vals)
        truncated-path-sets (mapfn
                             (fn [path-set] 
                               (filter (fn [path] 
                                         (not (some (fn [truncate-path]
                                                      (subpath? truncate-path path))
                                                    truncate-paths)))
                                       path-set))
                             path-sets)
        skeleton (first serialized)
        truncated-skeleton (dissoc-paths skeleton truncate-paths)
        truncated-serialized
        (cons truncated-skeleton
              (zipmap truncated-path-sets
                      path-vals))]
    (deserialize truncated-serialized)))

(defn truncate-expressions [expressions truncate-paths language-model]
  (map #(truncate % truncate-paths language-model)
       expressions))


(defn show-bolt [bolt language-model]
  (if (nil? bolt)
    (exception (str "don't call show-bolt with bolt=null."))
    (let [morph (:morph language-model)]
      (if (nil? morph)
        (exception (str "don't call show-bolt with morph=null."))
        (str (if (get-in bolt [:rule]) (str "[" (get-in bolt [:rule]) " "))
             (let [head-bolt (get-in bolt [:head])]
               (if (nil? head-bolt)
                 (morph bolt)
                 (let [rest-str (show-bolt (get-in bolt [:head]) language-model)]
                   (if (not (nil? rest-str))
                     (str "-> " rest-str)))))
             (if (get-in bolt [:rule]) "]"))))))

(defn subpath? [path1 path2]
  "return true if path1 is subpath of path2."
  (if (empty? path1)
    true
    (if (= (first path1) (first path2))
      (subpath? (rest path1)
                (rest path2))
      false)))
