(ns babel.over
  (:refer-clojure :exclude [get get-in resolve find parents])
  (:require
   [babel.exception :refer [exception]]
   [babel.lexiconfn :refer [get-fail-path]]
   [clojure.string :as string]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [dag_unify.core :refer [copy fail? fail-path fail-path-between get-in strip-refs unify unifyc
                           ;; temporary: until we move (truncate) from here to dag_unify.
                           deserialize dissoc-paths serialize
]]))

;; TODO: need better debugging throughout this file to diagnose generation failures.
;; using (get-fail-path) is one example.

;; use map or pmap.
(def ^:const mapfn pmap)

(def ^:dynamic *extra-diagnostics* false)

(defn check-vals [m1 m2 p1 p2]
  (let [v1 (get-in m1 p1 :top)
        v2 (get-in m2 p2 :top)
        result
        (cond (= :top v1)
              false
              (= :top v2)
              false
              (= :fail v1)
              true
              (= :top v2)
              true
              (and (map? m1)
                   (map? m2))
              (fail? (unifyc v1 v2))

              true
              (not (= v1 v2)))]
    (if (= result true)
      (log/debug (str "check-vals caught a fail: p1=" p1 "; p2=" p2 "; fail-path:" (fail-path m1 m2))))
    result))

(defn head-pre-checks [parent child]
  (or
   (check-vals parent child [:head :synsem :cat]            [:synsem :cat])
   (check-vals parent child [:head :synsem :essere]         [:synsem :essere])
   (check-vals parent child [:head :synsem :subcat :1 :cat] [:synsem :subcat :1 :cat])
   (check-vals parent child [:head :synsem :subcat :2 :cat] [:synsem :subcat :2 :cat])
   (check-vals parent child [:head :synsem :infl]           [:synsem :infl])
   (check-vals parent child [:head :phrasal]                [:phrasal])
   (check-vals parent child [:head :synsem :agr]            [:synsem :agr])
   (check-vals parent child [:head :synsem :sem]            [:synsem :sem])
   (check-vals parent child [:head :synsem :aux]            [:synsem :aux])
   (check-vals parent child [:head :synsem :pronoun]        [:synsem :pronoun])
   (check-vals parent child [:head :synsem :propernoun]     [:synsem :propernoun])
   (check-vals parent child [:head :synsem :subcat]         [:synsem :subcat])
   (check-vals parent child [:head :modified]               [:modified])
   ;; TODO: language-specific rules should be supplied from their respective namespaces.
   (check-vals parent child [:head :espanol :espanol]       [:espanol :espanol])
   (check-vals parent child [:head :italiano :italiano]     [:italiano :italiano])
   (check-vals parent child [:head :synsem :comp-type]      [:synsem :comp-type])

   ))

(defn comp-pre-checks [parent child]
  (or
   (check-vals parent child [:comp :synsem :cat]            [:synsem :cat])
   (check-vals parent child [:comp :synsem :agr]            [:synsem :agr])
   (check-vals parent child [:comp :synsem :case]           [:synsem :case])
   (check-vals parent child [:comp :synsem :sem]            [:synsem :sem])
   (check-vals parent child [:comp :synsem :reflexive]      [:synsem :reflexive])
   (check-vals parent child [:comp :synsem :pronoun]        [:synsem :pronoun])
   (check-vals parent child [:comp :synsem :subcat]         [:synsem :subcat])
   ))

(defn moreover-head [parent child & [morph]]
  (let [morph (if morph morph (fn [x] (str "(no morph function provided:" x)))]
    (log/trace (str "moreover-head (candidate) parent: [" (get-in parent [:rule]) "] '" (morph parent) "' sem:    " (strip-refs (get-in parent '(:synsem :sem) :no-semantics))))
    (log/trace (str "moreover-head (candidate) head child: [" (get-in parent [:child]) "] '" (morph child) "' sem:" (strip-refs (get-in child '(:synsem :sem) :top))))
    (let [head-pre-checks (head-pre-checks parent child)
          result
          (if head-pre-checks
            (do
              (log/trace (str "moreover-head: head failed prechecks for parent: " (get-in parent [:rule])))
              :fail)
            (unify
             (copy parent)
             {:head (copy child)}))]
      (if (not (fail? result))
        (let [debug (log/trace (str "moreover-head: " (get-in parent '(:rule)) " succeeded: " (get-in result [:rule])
                                    ":'" (morph result) "'"))
              debug
              (let [p-sc (get-in parent [:head :synsem :subcat :1 :cat] :top)
                    c-sc (get-in child [:synsem :subcat :1 :cat] :top)]
                (if (fail? (unifyc p-sc c-sc))
                  (do
                    (log/debug (str "moreover-head: pass: parent sc:" (get-in parent [:head :synsem :subcat :1 :cat] :none)))
                    (log/debug (str "moreover-head: pass: head sc:  " (get-in child [:synsem :subcat :1 :cat] :none))))))
              debug (log/trace (str " resulting sem: " (strip-refs (get-in result '(:synsem :sem)))))]
          result)

        ;; else: attempt to put head under parent failed: provide diagnostics through log/debug messages.
        ;; Ideally the attempt would always succeed, because calling (moreover-head) is expensive
        ;; because of the need to copy structures.
        (do
          (if (not (= true head-pre-checks))
            (log/warn (str "moreover-head: pre-check missed: fail-path-between "
                           "parent:'" (get-in parent [:rule]) "' and child with pred:"
                           (get-in child [:rule] (get-in child [:synsem :sem :pred] :nopred)) " ; "
                           (let [{path :fail-path
                                  parent-value :val1
                                  head-value :val2} (fail-path parent {:head child})]
                             (str "parent[" path        "] = " parent-value ";")))))
                             
          (if (= *extra-diagnostics* true)
            (let [fail-path (get-fail-path (get-in parent [:head]) child)]
              (log/trace (str "moreover-head: failed to add head: '" (morph child) "' to parent: " (get-in parent [:rule])))
              (log/trace (str "parent " (get-in parent [:rule])
                              " wanted head with: "
                              (strip-refs (get-in parent [:head :synsem]))))
              (log/trace (str "fail-path: " (get-fail-path (get-in parent [:head])
                                                           child)))
              (log/trace (str "  parent@" fail-path "="
                              (get-in parent (concat [:head] fail-path))))
              (log/trace (str "    head@" fail-path "="
                              (get-in child fail-path)))
              (if (and (not (= (get-in parent [:synsem :cat])
                               (get-in child [:synsem :cat]))))
                (log/warn (str "moreover-head: parent's :cat != head child's cat.")))
              (if (fail? (unify (get-in parent [:head :synsem :subcat :1 :cat])
                                (get-in child [:synsem :subcat :1 :cat])))
                (log/trace (str "moreover-head: parent specifies: [:head :synsem :subcat :1]: " (get-in parent [:head :synsem :subcat :1 :cat]) ";"
                                "moreover-head: head child specifies: [:synsem subcat :1]   : " (get-in child [:synsem :subcat :1 :cat]))))
              (if (and false ;; TODO (if no-pre-checks-have-caught-this ..)
                       (get-in parent [:head :synsem :infl])
                       (get-in comp [:synsem :infl]))
                (log/debug (str "moreover-head: fail-path-between:"
                                (fail-path-between parent {:head child}))))))
          :fail)))))

(defn overh
  "add given head as the head child of the phrase: parent."
  [parent head]
  (when (and (map? parent)
             (map? head))
    (do
      (log/trace (str "overh: parent: " (get-in parent [:rule])))
      (log/trace (str "overh: head: " (get-in head [:rule] (str "head is a lexeme with pred: " (strip-refs (get-in head [:synsem :sem :pred]))))))
      (log/trace (str "overh: parent: " parent))
      (log/trace (str "overh: head: " head))))
  ;; TODO: get rid of all this type-checking and use
  ;; whatever the smart people use for Clojure argument type-checking.
  (cond

   (nil? head)
   nil

   (or
    (seq? parent)
    (set? parent)
    (vector? parent))
   (let [parents (lazy-seq parent)]
     (filter (fn [result]
               (not (fail? result)))
             (mapcat (fn [each-parent]
                       (overh each-parent head))
                     parents)))

   (or (set? head)
       (vector? head))
   (do (log/trace "head is a set: converting to a seq.")
       (overh parent (lazy-seq head)))

   (seq? head)
   (let [head-children head]
     (log/trace (str "head is a seq - actual type is " (type head)))
     (filter (fn [result]
               (not (fail? result)))
             (mapcat (fn [child]
                       (overh parent child))
                     head-children)))
   true
   ;; TODO: 'true' here assumes that both parent and head are maps: make this assumption explicit,
   ;; and save 'true' for errors.
   (let [result (moreover-head parent head)
         is-fail? (fail? result)
         label (if (get-in parent [:rule]) (get-in parent [:rule]) (:comment parent))]
     (if (not is-fail?)
       (do
         (log/debug (str "overh: " (get-in parent [:rule]) " -> " (get-in head [:rule]
                                                                        (get-in head [:synsem :sem :pred]
                                                                                "(no pred for head)"))))
         (log/trace (str "overh successful result: " (strip-refs (dissoc result :dag_unify.core/serialized))))
         (list result))))))

;; Haskell-looking signature:
;; (parent:map) X (child:{set,seq,fs}) => list:map
;; TODO: verify that the above comment about the signature
;; is still true.
(defn overc [parent comp]
  "add given child as the comp child of the phrase: parent."

  (log/trace (str "set? parent:" (set? parent)))
  (log/trace (str "seq? parent:" (seq? parent)))
  (log/trace (str "seq? comp:" (seq? comp)))

  (log/trace (str "type of parent: " (type parent)))
  (log/trace (str "type of comp  : " (type comp)))
  (log/trace (str "nil? comp  : " (nil? comp)))

  (cond
   (nil? comp) nil

   (or
    (seq? parent)
    (set? parent)
    (vector? parent))
   (let [parents (lazy-seq parent)]
     (filter (fn [result]
               (not (fail? result)))
             (mapcat (fn [parent]
                       (overc parent comp))
                     parents)))

   #?(:clj (future? comp))
   #?(:clj (overc parent (deref comp)))

   (or (set? comp)
       (vector? comp))
   (do (log/trace "comp is a set: converting to a seq.")
       (overc parent (lazy-seq comp)))

   (seq? comp)
   (let [comp-children comp]
     (log/trace (str "comp is a seq - actual type is " (type comp)))
     (filter (fn [result]
               (not (fail? result)))
             (mapcat (fn [child]
                       (overc parent child))
                     comp-children)))
   true
   (let [check-result (comp-pre-checks parent comp)
         result (if check-result :fail
                      (unifyc parent
                              {:comp comp}))
         is-fail? (fail? result)]
     (if (not is-fail?)
       (do
         (log/debug (str "overc: " (get-in parent [:rule]) " -> " (get-in comp [:rule]
                                                                          (get-in comp [:synsem :sem :pred]
                                                                                  "(no pred for comp)"))))
         ;; TODO: why are we returning a list here rather than just the result?
         (list result))))))

(defn overhc [parent head comp]
  (-> parent
      (overh head)
      (overc comp)))

;; TODO: distinguish between when:
;; 1) called with only a child1 (no child2),
;; 2) called with both a child1 and a child2, but child2's supplied value is nil:
;;    should be treated the same as empty list.
(defn over [parents child1 & [child2]]
  (log/trace (str "over:" (count parents)))
  (cond (vector? child1)
        (over parents (seq child1) child2)

        (vector? child2)
        (over parents child1 (seq child2))

        (map? parents)
        (do
          (log/trace (str "parents is a map: converting to a list and re-calling."))
          (over (list parents) child1 child2))

        (nil? child2)
        (over parents child1 :top)        

        (empty? parents)
        nil

        (and (map? parents)
             (not (nil? (:dag_unify.core/serialized parents))))
        ;; In this case, supposed 'parent' is really a lexical item: for now, definition of 'lexical item' is,
        ;; it has a non-nil value for :dag_unify.core/serialized - just return nil, nothing else to do.
        ;; TODO: above test of 'lexical item' is not right: a parent might very well have a :serialized key.
        (throw (exception (str "Don't know what to do with this parent: " parents)))

        ;; if parent is a symbol, evaluate it; should evaluate to a list of expansions (which might also be symbols, etc).
        #?(:clj (symbol? parents))
        #?(:clj (over (eval parents) child1 child2))

        true
        (let [parent (first parents)] ;; TODO: use recur
          (cond
            (nil? (get-in parent [:schema-symbol] nil))
            (throw (exception (str "no schema symbol for parent: " (:rule parent))))

            true
            (let [[head comp] (if (= (:first parent) :head)
                                [child1 child2]
                                [child2 child1])]
              (log/trace (str "over: parent: " (get-in parent [:rule]) " (" (get-in parent [:schema-symbol]) "); heads:["
                              (string/join ","
                                           (map (fn [h]
                                                  (get-in h [:rule]
                                                          (str (get-in h [:synsem :sem :pred]))))
                                                head))
                              "]"))
              (if (= (:first parent) :head)
                ;; else, head is left child.
                (do (log/trace "over: left child (head):" (strip-refs child1))
                    (log/trace "over: right child (comp):" (strip-refs child2)))
                ;; else, head is right child.
                (do (log/trace "over: left child (comp):" (strip-refs child1))
                    (log/trace "over: right child (head):" (strip-refs child2))))

              (concat
               ;; if parent is map, do introspection: figure out the schema from the :schema-symbol attribute,
               ;; and figure out head-comp ordering from :first attribute.
               (filter (fn [each]
                         (not (fail? each)))
                       (overhc parent
                               (if (= (:first parent) :head)
                                 child1 child2)
                               (if (= (:first parent) :head)
                                 child2 child1)))
               (over (rest parents) child1 child2)))))))

(defn morph-with-recovery [morph-fn input]
  (if (nil? input)
    (exception (str "don't call morph-with-recovery with input=nil.")))
  (if (nil? morph-fn)
    (exception (str "don't call morph-with-recovery with morph-fn=nil.")))
  (let [result (morph-fn input)
        result (if (or (nil? result)
                       (= "" result))
                 (get-in input [:english :english] "")
                 result)
        result (if (or (nil? result)
                       (= "" result))
                 (get-in input [:english] "")
                 result)
        result (if (or (nil? result)
                       (= "" result))
                 (get-in input [:rule] "")
                 result)
        result (if (or (nil? result)
                       (= "" result))
                 (exception
                  (str "r5: " input "/" (nil? input)))
                 result)]
    result))

(defn show-bolt [bolt language-model]
  (if (nil? bolt)
    (exception (str "don't call show-bolt with bolt=null."))
    (let [morph (:morph language-model)]
      (if (nil? morph)
        (exception (str "don't call show-bolt with morph=null."))
        (str "[" (get-in bolt [:rule])
             " '" (morph-with-recovery morph bolt) "'"
             (let [head-bolt (get-in bolt [:head])]
               (if (not (nil? head-bolt))
                 (let [rest-str (show-bolt (get-in bolt [:head]) language-model)]
                   (if (not (nil? rest-str))
                     (str " -> " rest-str)))))
             "]")))))

(defn subpath? [path1 path2]
  "return true if path1 is subpath of path2."
  (if (empty? path1)
    true
    (if (= (first path1) (first path2))
      (subpath? (rest path1)
                (rest path2))
      false)))

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
