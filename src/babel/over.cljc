(ns babel.over
  (:refer-clojure :exclude [get get-in merge resolve find parents])
  (:require
   [clojure.string :as string]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [dag_unify.core :refer [copy fail? fail-path fail-path-between get-in merge strip-refs unify unifyc]]
   [babel.lexiconfn :refer [get-fail-path sem-impl]]))

(defn exception [error-string]
  #?(:clj
     (throw (Exception. error-string)))
  #?(:cljs
     (throw (js/Error. error-string))))

;; TODO: need better debugging throughout this file to diagnose generation failures.
;; using (get-fail-path) is one example.

;; tree-building functions: useful for developing grammars.
(defn into-list-of-maps [arg]
  (cond

   (seq? arg)
   arg

   (set? arg)
   (seq arg)

   (map? arg)
   (list arg)

   (nil? arg)
   (list :top)

   (keyword? arg)
   (list arg)

   true (throw (exception (str "into-map: don't know what to do with a " (type arg) ".")))))

(declare overh)
(declare overc)

(defn over-each-parent-head [parents head]
  (if (not (empty? parents))
    (let [each-parent (first parents)]
      (log/debug (str "over-each-parent-head: each-parent type:" (type (first parents))))
      (log/debug (str "over-each-parent-head: head type:" (type head)))
      (lazy-cat
       (overh each-parent head)
       (over-each-parent-head (rest parents) head)))
    (do
      (log/debug (str "over-each-parent-head: done. returning nil"))
      nil)))

(defn over-each-parent-comp [parents comp]
  (log/trace (str "over-each-parent-comp: parents type: " (type parents)))
  (log/trace (str "over-each-parent-comp: comp type: " (type comp)))
  (if (not (empty? parents))
    (let [each-parent (first parents)]
      (log/trace (str "over-each-parent-comp: each-parent type:" (type (first parents))))
      (log/trace (str "over-each-parent-comp: comp type:" (type comp)))
      (lazy-cat
       (overc each-parent comp)
       (over-each-parent-comp (rest parents) comp)))
    (do
      (log/trace (str "over-each-parent-comp: done. returning nil"))
      nil)))

(defn over-each-head-child [parent children morph]
  (log/trace (str "over-each-head-child: parent type: " (type parent)))
  (log/trace (str "over-each-head-child: head children type: " (type children)))
  (if (not (empty? children))
    (let [each-child (first children)]
      (lazy-cat
       (overh parent each-child morph)
       (over-each-head-child parent (rest children) morph)))
    (do
      (log/trace (str "over-each-head-child: done. returning nil."))
      nil)))

(defn over-each-comp-child [parent children]
  (log/trace (str "over-each-comp-child: parent type: " (type parent)))
  (log/trace (str "over-each-comp-child: comp children type: " (type children)))
  (if (not (empty? children))
    (let [each-child (first children)]
      (lazy-cat
       (overc parent each-child)
       (over-each-comp-child parent (rest children))))
    (do
      (log/trace (str "over-each-comp-child: done. returning nil."))
      nil)))

(def ^:dynamic *extra-diagnostics* false)
(def ^:dynamic *check-parent-and-head-child-cat-equal* true)
(def ^:dynamic *check-infl* true)

(defn head-pre-checks [parent child]
  (log/debug (str "head-pre-checks PH: " (strip-refs (get-in parent [:head]))))
  (log/debug (str "head-pre-checks H: " (dissoc (strip-refs child) :serialized)))
  (or
   (fail? (unifyc (get-in parent [:head :synsem :infl] :top)
                  (get-in child [:synsem :infl] :top)))
   (fail? (unifyc (get-in parent [:head :synsem :subcat] :top)
                  (get-in child [:synsem :subcat] :top)))
   (fail? (unifyc (get-in parent [:head :synsem :agr] :top)
                  (get-in child [:synsem :agr] :top)))
   (fail? (unifyc (get-in parent [:head :phrasal] :top)
                  (get-in child [:phrasal] :top)))
   (fail? (unifyc (get-in parent [:head :synsem :sem] :top)
                  (get-in child [:synsem :sem] :top)))
   (fail? (unifyc (get-in parent [:head :synsem :aux] :top)
                  (get-in child [:synsem :aux] :top)))
   (fail? (unifyc (get-in parent [:head :synsem :cat] :top)
                  (get-in child [:synsem :cat] :top)))
   (fail? (unifyc (get-in parent [:head :synsem :pronoun] :top)
                  (get-in child [:synsem :pronoun] :top)))
   (fail? (unifyc (get-in parent [:head :synsem :subcat :1] :top)
                  (get-in child [:synsem :subcat :1] :top)))
   (fail? (unifyc (get-in parent [:head :synsem :subcat :2] :top)
                  (get-in child [:synsem :subcat :2] :top)))))

(defn comp-pre-checks [parent child]
  (or
   (fail? (unifyc (get-in parent [:comp :synsem :cat] :top)
                  (get-in child [:synsem :cat] :top)))
   (fail? (unifyc (get-in parent [:comp :synsem :agr] :top)
                  (get-in child [:synsem :agr] :top)))
   (fail? (unifyc (get-in parent [:comp :synsem :case] :top)
                  (get-in child [:synsem :case] :top)))
   (fail? (unifyc (get-in parent [:comp :synsem :sem] :top)
                  (get-in child [:synsem :sem] :top)))
   (fail? (unifyc (get-in parent [:comp :synsem :reflexive] :top)
                  (get-in child [:synsem :reflexive] :top)))
   (fail? (unifyc (get-in parent [:comp :synsem :pronoun] :top)
                  (get-in child [:synsem :pronoun] :top)))
   (fail? (unifyc (get-in parent [:comp :synsem :subcat] :top)
                  (get-in child [:synsem :subcat] :top)))))

(defn moreover-head [parent child lexfn-sem-impl morph]
  (let [morph (if morph morph (fn [x] (strip-refs (dissoc x :serialized))))]
    (log/debug (str "moreover-head (candidate) parent: [" (get-in parent [:rule]) "] '" (morph parent) "' sem:    " (strip-refs (get-in parent '(:synsem :sem) :no-semantics))))
    (log/debug (str "moreover-head (candidate) head child: [" (get-in parent [:child]) "] '" (morph child) "' sem:" (strip-refs (get-in child '(:synsem :sem) :top))))
    (let [head-pre-checks (head-pre-checks parent child)
          result
          (if head-pre-checks
            (do
              (log/debug (str "head failed prechecks."))
              :fail)
            (unify
             (copy parent)
             (unify {:head (copy child)
                     :head-filled true}
                    {:head {:synsem {:sem (lexfn-sem-impl (copy (get-in child '(:synsem :sem) :top)))}}})))]
      (if (not (fail? result))
        (let [debug (log/trace (str "moreover-head: " (get-in parent '(:rule)) " succeeded: " (get-in result [:rule])
                                    ":'" (morph result) "'"))
              debug
              (let [p-sc (get-in parent [:head :synsem :subcat :1 :cat] :top)
                    c-sc (get-in child [:synsem :subcat :1 :cat] :top)]
                (if (fail? (unify p-sc c-sc))
                  (do
                    (log/debug (str "moreover-head: pass: parent sc:" (get-in parent [:head :synsem :subcat :1 :cat] :none)))
                    (log/debug (str "moreover-head: pass: head sc:  " (get-in child [:synsem :subcat :1 :cat] :none))))))
              debug (log/trace (str " resulting sem: " (strip-refs (get-in result '(:synsem :sem)))))]
          result)

        ;; else: attempt to put head under parent failed: provide diagnostics through log/debug messages.
        (do
          (if (not (= true head-pre-checks))
            (log/warn (str "moreover-head: pre-checked missed: fail-path-between:"
                           (fail-path-between parent {:head child}))))
          (if (= *extra-diagnostics* true)
            (let [fail-path (get-fail-path (get-in parent [:head]) child)]
              (log/trace (str "moreover-head: failed to add head: '" (morph child) "' to parent: " (get-in parent [:rule])))
              (log/trace (str "parent " (get-in parent [:rule])
                              " wanted head with: "
                              (strip-refs (get-in parent [:head :synsem]))))
              (log/trace (str "candidate child has synsem: "
                              (strip-refs
                               (get-in
                                (unifyc child
                                        {:synsem {:sem (lexfn-sem-impl (get-in child '(:synsem :sem) :top))}})
                                [:synsem]))))
              (log/trace (str "fail-path: " (get-fail-path (get-in parent [:head])
                                                           child)))
              (log/trace (str "  parent@" fail-path "="
                              (get-in parent (concat [:head] fail-path))))
              (log/trace (str "    head@" fail-path "="
                              (get-in child fail-path)))
              (if (and (not (= (get-in parent [:synsem :cat])
                               (get-in child [:synsem :cat]))))
                (log/warn (str "moreover-head: CHILD CAT DIFFERS FROM HEAD CAT!")))
              (if (fail? (unify (get-in parent [:head :synsem :subcat :1 :cat])
                                (get-in child [:synsem :subcat :1 :cat])))
                (log/trace (str "moreover-head: SUBCAT parent head subcat 1:" (get-in parent [:head :synsem :subcat :1 :cat]) ";"
                                "moreover-head: SUBCAT        head subcat 1:" (get-in child [:synsem :subcat :1 :cat]))))
              (if (and false ;; TODO (if no-pre-checks-have-caught-this ..)
                       (get-in parent [:head :synsem :infl])
                       (get-in comp [:synsem :infl]))
                (log/debug (str "moreover-head: fail-path-between:"
                                (fail-path-between parent {:head child}))))))
          :fail)))))

;; Might be useful to set the following variable to true,
;; if doing grammar development and it would be unexpected
;; to have a failing result from calling (moreover-comp)
;; with certain arguments.
(def ^:dynamic *throw-exception-if-failed-to-add-complement* false)

(defn moreover-comp [parent child lexfn-sem-impl]
  (log/debug (str "moreover-comp type parent: " (type parent)))
  (log/debug (str "moreover-comp type comp:" (type child)))
  (log/debug (str "moreover-comp: child cat: " (get-in child [:synsem :cat])))
  (log/debug (str "moreover-comp: parent comp cat: " (get-in parent [:comp :synsem :cat])))
  
  (let [pre-check (comp-pre-checks parent child)
        result
        (if pre-check
          (do
            (log/debug (str "child failed pre-check."))
            :fail)
          (unifyc parent
                  {:comp child}
                  {:comp {:synsem {:sem (lexfn-sem-impl (get-in child '(:synsem :sem) :top))}}}))]
    (if (not (fail? result))
      (do (log/debug "unification was successful.")
          (merge {:comp-filled true}
                 result))
      ;; else: fail:
      (do
        (log/trace (str "moreover-comp: fail: " result))
        (if (= false pre-check)
          (log/warn (str "moreover-comp: pre-checked missed: fail-path-between:"
                         (fail-path-between parent {:comp child}))))
        (log/trace (str "moreover-comp: fail: child: " (strip-refs child)))
        (if (and
             *throw-exception-if-failed-to-add-complement*
             (get-in child '(:head)))
          (throw (exception (str "failed to add complement: " child "  to: phrase: " parent
                                 ". Failed path was: " (fail-path result)
                                 ". Value of parent at path is: "
                                 (get-in parent (fail-path result))
                                 "; Synsem of child is: "
                                 (get-in child '(:synsem) :top)))))
        (log/trace (str "moreover-comp: complement synsem: " (strip-refs (get-in child '(:synsem) :top))))
        (log/trace (str "moreover-comp:  parent value: " (strip-refs (get-in parent (fail-path result)))))
        :fail))))

(defn overh
  "add given head as the head child of the phrase: parent."
  [parent head morph]
  (when (and (map? parent)
             (map? head))
    (do
      (log/debug (str "overh: parent: " (strip-refs (dissoc parent :serialized))))
      (log/debug (str "overh: head: " (strip-refs (dissoc head :serialized))))))

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
             (over-each-parent-head parents head)))

   (or (set? head)
       (vector? head))
   (do (log/trace "head is a set: converting to a seq.")
       (overh parent (lazy-seq head) morph))

   (seq? head)
   (let [head-children head]
     (log/trace (str "head is a seq - actual type is " (type head)))
     (filter (fn [result]
               (not (fail? result)))
             (over-each-head-child parent head-children morph)))

   true
   ;; TODO: 'true' here assumes that both parent and head are maps: make this assumption explicit,
   ;; and save 'true' for errors.
   (let [result (moreover-head parent head sem-impl morph)
         is-fail? (fail? result)
         label (if (get-in parent [:rule]) (get-in parent [:rule]) (:comment parent))]
     (if (not is-fail?)
       (do
         (log/debug (str "overh successful result: " (strip-refs (dissoc result :serialized))))
         (list result))))))

;; Haskell-looking signature:
;; (parent:map) X (child:{set,seq,fs}) => list:map
;; TODO: verify that the above commentn about the signature
;; is still true.
(defn overc [parent comp]
  "add given child as the comp child of the phrase: parent."

  (log/debug (str "set? parent:" (set? parent)))
  (log/debug (str "seq? parent:" (seq? parent)))
  (log/debug (str "seq? comp:" (seq? comp)))

  (log/debug (str "type of parent: " (type parent)))
  (log/debug (str "type of comp  : " (type comp)))
  (log/debug (str "nil? comp  : " (nil? comp)))

  (cond
   (nil? comp) nil

   (or
    (seq? parent)
    (set? parent)
    (vector? parent))
   (let [parents (lazy-seq parent)]
     (filter (fn [result]
               (not (fail? result)))
             (over-each-parent-comp parents comp)))

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
             (over-each-comp-child parent comp-children)))

   true
   (let [result (moreover-comp parent comp sem-impl)
         is-fail? (fail? result)]
     (if (not is-fail?)
       (list result)))))

(defn overhc [parent head comp & [morph]]
  (overc (overh parent head morph) comp))

;; TODO: distinguish between when:
;; 1) called with only a child1 (no child2),
;; 2) called with both a child1 and a child2, but child2's supplied value is nil:
;;    should be treated the same as empty list.
(defn over [parents child1 & [child2]]
  (cond (vector? child1)
        (over parents (seq child1) child2)

        (vector? child2)
        (over parents child1 (seq child2))

        (map? parents)
        (do
          (log/debug (str "parents is a map: converting to a list and re-calling."))
          (over (list parents) child1 child2))

        (nil? child2)
        (over parents child1 :top)        

        (empty? parents)
        nil

        (and (map? parents)
             (not (nil? (:serialized parents))))
        ;; In this case, supposed 'parent' is really a lexical item: for now, definition of 'lexical item' is,
        ;; it has a non-nil value for :serialized - just return nil, nothing else to do.
        (throw (exception (str "Don't know what to do with this parent: " parents)))

        ;; if parent is a symbol, evaluate it; should evaluate to a list of expansions (which might also be symbols, etc).
        #?(:clj (symbol? parents))
        #?(:clj (over (eval parents) child1 child2))

        true
        (let [parent (first parents)]
          (cond
            (nil? (get-in parent [:schema-symbol] nil))
            (throw (exception (str "no schema symbol for parent: " (:rule parent))))

            true
            (do
              (log/debug (str "over: parent: " (get-in parent [:rule]) " with schema symbol: " (get-in parent [:schema-symbol])))
              (if (= (:first parent) :head)
                ;; else, head is left child.
                (do (log/debug "over: left child (head):" child1)
                    (log/debug "over: right child (comp):" child2))
                ;; else, head is right child.
                (do (log/debug "over: left child (comp):" child1)
                    (log/debug "over: right child (head):" child2)))

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




