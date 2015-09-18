(ns babel.html
  (:use
   [hiccup core page]
   [ring.util.codec :as codec])
  (:require
   [clj-time.coerce :as c]
   [clj-time.core :as t]
   [clj-time.format :as f]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [environ.core :refer [env]]
   [hiccup.element :as e]
   [hiccup.page :as h]
   [dag-unify.core :as fs]))

(def menubar-enabled true)

(defn verb-row [italian]
  (html
   [:tr
    [:th italian] [:td "FAIL." ]
    ]))

(defn verb-table [lexicon]
  (html [:table
	(for [verb (sort (keys lexicon))]
	     (verb-row verb))]))

(defn- url-escape
 "Like clojure.core/str but escapes ',\", ..(maybe more)."
 [x]
  (-> x str (.replace "'" "%27")))

(defn google-translate [italian]
  (str
   "<a href='"
   "http://translate.google.com/"
   "#it|en|"
   (codec/url-encode italian)
   "'"   ">"
   italian "</a>"))

(defn static-page [body & [title]]
  "create a self-contained html page (for use with file:/// urls)."
  (html
   [:html
    [:head
     [:meta  {:Content-Type "text/html; charset=UTF-8"}]
     [:title (str title
                  (if (and title (not (= title "")))
                    ": " "")
                  "imparare l'italiano")]

     (include-css "resources/public/css/fs.css")
     (include-css "resources/public/css/layout.css")
     (include-css "resources/public/css/quiz.css")

     (h/include-css "resources/public/css/bootstrap.min.css")
     (h/include-css "resources/public/css/prettify.css")

     (include-css "http://maxcdn.bootstrapcdn.com/font-awesome/4.1.0/css/font-awesome.min.css")

     ]

    [:body
     body]]))

(defn enumerate-serialized-paths [paths n]
  (if paths
    (let [path (first paths)]
      (if path
        (conj
         {(keyword (str "path" n))
          (str (string/join " " path))}
         (enumerate-serialized-paths (rest paths) (+ 1 n)))
        {}))
    {}))

(defn enumerate-serialized [arg n]
  (if arg
    (let [elem (first arg)]
      (if elem
        (conj
         {(keyword (str "p" n))
          {:paths (if (not (nil? (first elem)))
                    (enumerate-serialized-paths (first elem) 0))
           :skel (second elem)}}
         (enumerate-serialized (rest arg) (+ 1 n)))
        {}))
    {}))

(defn tablize-ser [arg]
  (enumerate-serialized arg 0))

(defn head-initial? [arg path]
  (let [retval
  (and
   (map? arg)
   
   (not (= :subcat (last path)))
   (not (= :italiano (last path)))
   (not (= :français (last path)))
   (not (= :english (last path)))
   
   (not (= :none (:head arg :none)))
   (not (= :none (:comp arg :none)))
   (= :none (:1 arg :none))
   (= :none (:2 arg :none))
   
   (or
    (and
     (not (= :none (fs/get-in arg '(:head :italiano) :none)))
     (not (= :none (fs/get-in arg '(:comp :italiano) :none))))
    (and
     (not (= :none (fs/get-in arg '(:head :français) :none)))
     (not (= :none (fs/get-in arg '(:comp :français) :none))))
    (and
     (not (= :none (fs/get-in arg '(:head :english) :none)))
     (not (= :none (fs/get-in arg '(:comp :english) :none)))))
   
   (or
    (and (fs/ref= arg '(:head :italiano) '(:italiano :a))
         (fs/ref= arg '(:comp :italiano) '(:italiano :b)))
    (and (fs/ref= arg '(:head :français) '(:français :a))
         (fs/ref= arg '(:comp :français) '(:français :b)))
    (and (fs/ref= arg '(:head :english) '(:english :a))
         (fs/ref= arg '(:comp :english) '(:english :b)))))]
    retval))

(defn head-final? [arg path]
  (let [retval
  (and
   (map? arg)
   
   (not (= :subcat (last path)))
   (not (= :italiano (last path)))
   (not (= :english (last path)))
   (not (= :français (last path)))
   
   (not (= :none (:head arg :none)))
   (not (= :none (:comp arg :none)))
   (= :none (:1 arg :none))
   (= :none (:2 arg :none))
   
   (or
    (and
     (not (= :none (fs/get-in arg '(:head :italiano) :none)))
     (not (= :none (fs/get-in arg '(:comp :italiano) :none))))
    (and
     (not (= :none (fs/get-in arg '(:head :français) :none)))
     (not (= :none (fs/get-in arg '(:comp :français) :none))))
    (and
     (not (= :none (fs/get-in arg '(:head :english) :none)))
     (not (= :none (fs/get-in arg '(:comp :english) :none)))))
   
   (or
    (and (fs/ref= arg '(:head :italiano) '(:italiano :b))
         (fs/ref= arg '(:comp :italiano) '(:italiano :a)))
    (and (fs/ref= arg '(:head :français) '(:français :b))
         (fs/ref= arg '(:comp :français) '(:français :a)))
    (and (fs/ref= arg '(:head :english) '(:english :b))
         (fs/ref= arg '(:comp :english) '(:english :a)))))]
    retval))

(def show-top true)
(def show-true true)

;; must be a set, not just a vector, so that contains? works as expected.
(def hide-these-features (set [:comp-filled
                               :a :b
                               :activity
                               :animate
                               :buyable
                               :drinkable
                               :edible
                               :futuro
                               :imperfect
                               :furniture
                               :legible
                               :mass
                               :part-of-human-body
                               :physical-object
                               :place
                               :present
                               :speakable
                               :artifact]))

;; TODO: use multimethod based on arg's type.
(defn tablize [arg & [path serialized opts]]
 ;; set defaults.
  ;; (TODO: in which contexts are we passing an already-serialized arg?)
  ;; if not already serialized, then serialize:
  (let [serialized (if (nil? serialized)
                     (fs/serialize arg)
                     serialized) ;; .. if already serialized, use that.
        opts (if (nil? opts)
               {:as-tree true})]
    (cond
     (nil? arg) (str "<i>nil</i>")
     (= arg '()) (str "<i>&lt;&nbsp;&gt;</i>")
     (= (type arg) clojure.lang.LazySeq)
     (str
      ;; TODO: pass along additional args (path,serialized,opts)
      ;; to recursive tablize call. (TODO applies to all 3 of the
      ;; following conditional disjuncts).
      (clojure.string/join ""
                           (map (fn [each-arg]
                                  (tablize each-arg path (fs/serialize each-arg) opts))
                                (seq arg))))
     (set? arg)
     (reduce #'str
             (concat (list "<div class='set'><div class='delimiter'>{</div><div class='member'>")
                     (string/join "</div><div class='delimiter'>,</div><div class='member'>"
                                  (map (fn [each]
                                         (tablize each path (fs/serialize each) opts))
                                       (seq arg)))
                     (list "</div><div class='delimiter'>}</div></div>")))

     (vector? arg)
     (string/join ""
                  (map (fn [each]
                         (tablize each path (fs/serialize each) opts))
                       arg))

     (or (list? arg)
         (= (type arg) clojure.lang.Cons))
     (string/join ""
                  (map (fn [each]
                         (tablize each path (fs/serialize each) opts))
                       arg))

     ;; displaying a phrase structure tree (2 children)
     ;; Head-initial (H C)
     (head-initial? arg path)
     (str
      "<div class='phrase'>"
      "  <table class='phrase'>"
      "    <tr>"
      "      <td class='parent2child'>&nbsp;</td><td class='parent2child parent' colspan='5'>"
      (tablize (dissoc (dissoc arg :head) :comp) path serialized opts)
      "      </td><td class='parent2child'>&nbsp;</td>"
      "    </tr>"
      "    <tr>"
      "      <td class='ref'>"

      (if (= (type (:head arg)) clojure.lang.Ref)
        (str
         "     <div class='ref'>"
         (fs/path-to-ref-index serialized (concat path '(:head)) 0)
         "     </div>"))

      "      </td>"
      "      <td class='hc'>H</td><td>"

      (tablize (if (= (type (:head arg)) clojure.lang.Ref)
                 @(:head arg)
                 (:head arg))
               (concat path '(:head)) serialized opts)
      "      </td>"
      "      <td class='ref'>"

      (if (= (type (:comp arg)) clojure.lang.Ref)
        (str
         "    <div class='ref'>"
         (fs/path-to-ref-index serialized (concat path '(:comp)) 0)
         "    </div>"))

      "      </td>"
      "      <td class='hc'>C</td><td>"
      (tablize (if (= (type (:comp arg)) clojure.lang.Ref)
                 @(:comp arg)
                 (:comp arg))
               (concat path '(:comp)) serialized opts)
      "      </td><td>&nbsp;</td>"
      "    </tr>"
      "  </table>"
      "</div>")


     ;; displaying a phrase structure tree (2 children)
     ;; Head-final (C H)
     (head-final? arg path)
     (str
      "<div class='phrase'>"
      "  <table class='phrase'>"
      "    <tr>"
      "      <td class='parent2child'>&nbsp;</td><td class='parent2child parent' colspan='5'>"
      (tablize (dissoc (dissoc arg :head) :comp) path serialized opts)
      "      </td><td class='parent2child'>&nbsp;</td>"
      "    </tr>"
      "    <tr>"
      "      <td class='ref'>"
      (if (= (type (:comp arg)) clojure.lang.Ref)
        (str
         "     <div class='ref'>"
         (fs/path-to-ref-index serialized (concat path '(:comp)) 0)
         "     </div>"))
      "      </td>"
      "      <td class='hc'>C</td><td>"
      (tablize (if (= (type (:comp arg)) clojure.lang.Ref)
                 @(:comp arg)
                 (:comp arg))
               (concat path '(:comp)) serialized opts)
      "      </td>"
      "      <td class='ref'>"
      (if (= (type (:head arg)) clojure.lang.Ref)
        (str
         "    <div class='ref'>"
         (fs/path-to-ref-index serialized (concat path '(:head)) 0)
         "    </div>"))
      "      </td>"
      "      <td class='hc'>H</td><td>"
      (tablize (if (= (type (:head arg)) clojure.lang.Ref)
                 @(:head arg)
                 (:head arg))
               (concat path '(:head)) serialized opts)
      "      </td><td>&nbsp;</td>"
      "    </tr>"
      "  </table>"
      "</div>")


     ;; displaying a phrase structure tree (2 children)
     (and
      (map? arg)

      (not (= :subcat (last path)))

      ;; display :extends properly (i.e. not a tree).
      ;; :extends will have features :a,:b,:c,..
      (not (= :a (last path)))
      (not (= :b (last path)))
      (not (= :c (last path)))
      (not (= :d (last path)))
      (not (= :e (last path)))
      (not (= :f (last path)))
      (not (= :g (last path)))

      (not (= :none (:1 arg :none)))
      (not (= :none (:2 arg :none))))

     (str
      "<div class='phrase'>" "PHRASE"
      "  <table class='phrase'>"
      "    <tr>"
      "      <td class='parent2child'>&nbsp;</td><td class='parent2child parent' colspan='3'>"
      (tablize (dissoc (dissoc arg :1) :2) path serialized opts)
      "      </td><td class='parent2child'>&nbsp;</td>"
      "    </tr>"
      "    <tr>"
      "      <td class='ref'>"
      (if (= (type (:1 arg)) clojure.lang.Ref)
        (str
         "     <div class='ref'>"
         (fs/path-to-ref-index serialized (concat path '(:1)) 0)
         "     </div>"))
      "      </td>"
      "      <td>"
      (tablize (if (= (type (:1 arg)) clojure.lang.Ref)
                 @(:1 arg)
                 (:1 arg))
               (concat path '(:1)) serialized opts)
      "      </td>"
      "      <td class='ref'>"
      (if (= (type (:2 arg)) clojure.lang.Ref)
        (str
         "    <div class='ref'>"
         (fs/path-to-ref-index serialized (concat path '(:2)) 0)
         "    </div>"))
         "      </td>"
         "      <td>"
      (tablize (if (= (type (:2 arg)) clojure.lang.Ref)
                 @(:2 arg)
                 (:2 arg))
               (concat path '(:2)) serialized opts)
      "      </td><td>&nbsp;</td>"
      "    </tr>"
      "  </table>"
      "</div>")

    ;; displaying a phrase structure tree (1 child)
     (and
      (or true (not (nil? opts)))
      (or true (= true (:as-tree opts)))
      (map? arg)
      (not (= :subcat (last path)))

      (not (= :a (last path)))
      (not (= :b (last path)))
      (not (= :c (last path)))


      (not (= :none (:1 arg :none)))
      (= :none (:2 arg :none)))
     (str
      "<div class='phrase'>"
      "  <table class='phrase'>"
      "    <tr>"
      "      <td colspan="2" class='parent1child'>" (tablize (dissoc (dissoc arg :1) :2) path serialized {:as-tree false}) "</td>"
      "    </tr>"
      "    <tr>"
      "      <td>"
      (if (= (type (:1 arg)) clojure.lang.Ref)
        (str
         "    <div class='ref'>"
         (fs/path-to-ref-index serialized (concat path '(:1)) 0)
         "    </div>"))
         "      </td>"
         "      <td>"
      (tablize (if (= (type (:1 arg)) clojure.lang.Ref)
                 @(:1 arg) (:1 arg))
               (concat path (list :1))
               serialized opts)
      "      </td>"
      "    </tr>"
      "  </table>"
      "</div>")

     ;; displaying a feature structure.
     (map? arg)
     (str
      "<div class='map'>"
      (if (:header arg) (str "<h2>" (:header arg) "</h2>"))
      "  <table class='map'>"
      (string/join
       ""
       (map
        (fn [tr]
          (let [feature (first tr)]
            (str
             "<tr"
             (cond
              ;; TODO: more general method to allow passage of css info from other parts of code:
              (= (first tr) :paths)
              " style='white-space:nowrap;'"
              ;; use a custom CSS class for :comment.
              (= (first tr) :comment)
              " class='comment'"
              ;; use a custom CSS class for :header (i.e. hide it with display:none)
              (= (first tr) :header)
              " class='hide' style='display:none'"
              ;; ..handle other keywords that need a custom CSS class..
              ;; default: no custom CSS class.
              true "")
             ">"
             "<th>" feature "</th>"
             (if (= (type (second tr)) clojure.lang.Ref)
               (str
                "<td class='ref'>"
                "  <div class='ref'>"
                (fs/path-to-ref-index serialized (concat path (list (first tr))) 0)
                "  </div>"
                "</td>"
                "<td class='ref'>"
                )
               " <td class='ref' colspan='2'>")
             (tablize (second tr)
                      ;; set 'path' param for recursive call to tablize.
                      ;; Path' = Path . current_feature
                      (concat path (list (first tr)))
                      serialized
                      {:as-tree false})
             "   </td>"
             "</tr>")))
        ;; sorts the argument list in _arg__ by key name and remove uninteresting key-value pairs.
        (remove #(or (contains? hide-these-features
                                (first %))
                     ;; TODO: all the below should go into hide-these-features.
                     (= (first %) :aliases)
                     (= (first %) :case)
                     (= (first %) :comment)
                     (= (first %) :comment-plaintext)
                     (= (first %) :extend)
                     (= (first %) :pronoun)
                     (= (first %) :propernoun)
                     (= (first %) :first)
                     (= (first %) :head-filled)
                     (= (first %) :initial)
                     (= (first %) :phrasal)
                     (= (first %) :root)
                     (= (first %) :schema-symbol)
                     (= (first %) :serialized)
                     (and (not show-true)
                          (fs/ref? (second %))
                          (= @(second %) false))
                     (and (not show-top)
                          (fs/ref? (second %))
                          (= @(second %) :top))
                     (and (not show-true) (= (second %) false))
                     (and (not show-top) (= (second %) :top))

                     (and (not show-true) (fs/ref? (second %))
                          (= @(second %) true))
                     (and (not show-true) (fs/ref? (second %))
                          (= @(second %) :top))
                     (and (not show-true) (= (second %) true))
                     (and (not show-top) (= (second %) :top)))

                (into (sorted-map) arg))
        ))
      "  </table>"
      "</div>")
     (set? arg)
     (str
      "{"
      (clojure.string/join ","
                           (map (fn [member]
                                  (tablize member
                                           ;; set 'path' param for recursive call to tablize.
                                           ;; Path' = Path . current_feature
                                           (concat path (list (first member)))
                                           serialized
                                           {:as-tree false}
                                           ))
                                arg))
      "}")
     (= nil arg)
     (str "<div class='atom'><i>nil</i></div>")

     (= (last path) :rule)
     (str "<span class='keyword'>" arg "</span>")

     (= (type arg)
        java.lang.String)
     (str "<span class='string'>" arg "</span>")
     (= (type arg)
        java.lang.Long)
     (str "<span class='atom'>" arg "</span>")
     (= arg :fail)
     (str "<span class='keyword fail'>" arg "</span>")
     (= (type arg)
        clojure.lang.Keyword)
     (str "<span class='keyword'>" arg "</span>")
     (= (type arg)
        java.lang.Boolean)
     (str "<span class='boolean'>" arg "</span>")

     (or
         (= (type arg)
            java.lang.Integer)
         (= (type arg)
            java.lang.Double))
     (str "<span class='atom'>" arg "</span>")

     (and (= (type arg) clojure.lang.Ref)
          (= @arg nil))
     (str "NIL.")

     (symbol? arg)
     (str "<i>" arg "</i>")

     (= (type arg) clojure.lang.Ref)
     (let [is-first (fs/is-first-path serialized path 0
                                      (fs/path-to-ref-index serialized path 0))]
       (str (if (and (or (= (last path) :subcat)
                         (= is-first true))
                     (or false (not (= (last path) :head)))
                     (or false (not (= (last path) :comp))))
              (tablize @arg path serialized
                       (merge opts {:as-tree false})))))

     (fn? arg)
     "&lambda;"


     ;; TODO: should support objects of any otherwise-unsupported class: simply print out the classname in fixed-width font.
     (= (type arg) org.eclipse.jetty.server.HttpInput)
     "<tt>org.eclipse.jetty.server.HttpInput</tt>"

     true
     (str "<div class='unknown'>" "<b>don't know how to tablize this object : (type:" (type arg) "</b>;value=<b>"  arg "</b>)</div>"))))

(defn tablize-key-row [key val]
  (str
   "<tr>"
   (str "<th class='complex'>" (tablize key) "</th>")
   (str "<td class='complex'>"
        "<table class='list'>"
        "<tr><td>"
        (string/join
         "</td><td class='each'>"
         (map (fn [each-val]
                (tablize each-val))
              val))
        "</td></tr>"
        "</table>"
        "</td>")
   "</tr>"))

;; TODO: fold into tablize
(defn tablize-with-complex-keys [arg]
  (let [keys (keys arg)
        tablized-keys (map (fn [key]
                             (tablize key))
                           keys)]
    (str
     "<table class='complex'>"
     (string/join ""
           (map (fn [key]
                  (tablize-key-row key (get arg key)))
                keys))
     "</table>")))

(defn simple-fs []
  {:foo "bar"})

(defn nested-fs []
  {:foo {:bar {:baz "biff"}}})

(defn create-anchor [package test]
  (codec/url-encode (str package ":" test)))

(defn iframe [url]
  (str "<iframe src=\""  url "\"></iframe>"))

;; TODO: look at hiccup.page-helpers/doctype
(defn showdoctype [ & [type] ]
  (cond
   (= type "html5")
   "<!DOCTYPE html>"
   true ;; default is xhtml transitional (for now).
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
	\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"))

(defn myhtml5 []
  "<!DOCTYPE html>")

(defn welcome [username]
  (html
   [:div
    (if username
      [:div "Benvenuti, " username "."
       [:a {:href "/session/clear/"} "Logout"]
       ]
      [:a {:href "/session/set/"} "Login"]
      )]))

(defn powered-by [name link]
  (html
   [:div {:class "poweredby"}
    [:a {:href link}
     name ]]))

(defn includes-js [includes]
  (if (not (empty? includes))
    (do
      (include-js (first includes))
      (includes-js (rest includes)))))

(defn pretty-head [title]
  [:head 
   [:meta {:http-equiv "Content-Type" :content "text/html; charset=utf-8"}]

   (include-css "/css/bootstrap.min.css")
   (include-css "/css/fs.css")
   (include-css "/css/workbook.css")

   [:script {:type "text/javascript" :src "/js/jquery-1.6.4.min.js"}]
   [:script {:type "text/javascript" :src "/js/workbook.js"}]

   [:title (str title
                (if (and title (not (= title "")))
                  ": " "")
                "Verbcoach")]])

(defn pretty-body [ options & content ]
  [:body
   {:onload (if (:onload options) (:onload options) "")}
   (into [:div {:class "columns small-12"}] content)])

(defn page-body [content req & [title options]]
  (let [title (if title title "default page title")]
    (log/debug (str "page-body with options: " options))
    (string/join (map (fn [key]
                        (log/trace (str "env key: " key " => " (get env key))))
                      (sort (keys env)))
                 "")
    (h/html5
     (pretty-head title)
     (pretty-body
      options
      (:show-login-form options)
      content
      ))))

(defn page [title & [content request resources]]
  (let [resources (if (not (nil? resources)) (resources request))]
    (page-body
     (html
      (if (and request (:query-params request) (get (:query-params request) "result"))
        [:div {:class "fadeout"}
         (get (:query-params request) "result")])
      (if menubar-enabled (:menubar resources))
      [:div#content content]
      )
     request title resources)))

(declare tr)
(def short-format
  (f/formatter "MMM dd, yyyy HH:mm"))
(defn display-time [timestamp]
  "convert time objects of various types to our chosen datetime format."
  (cond (= (type timestamp) java.sql.Timestamp)
        (f/unparse short-format (c/from-long (.getTime timestamp)))
        (= (type timestamp) org.joda.time.DateTime)
        (f/unparse short-format timestamp)
        true
        timestamp))

(declare default-td)
(declare default-th)

(defn table [rows & {:keys [columns
                            create-new
                            haz-admin
                            th
                            td
                            none]}]
  (let [columns (if columns columns
                    (if (not (empty? rows))
                      (keys (first rows))))
        haz-admin (if haz-admin haz-admin false)
        none (if none none "No results.")
        th (if th th (fn [key] (default-th key)))
        td (if td td 
               (fn [row key] (default-td row key)))]
    (html
     (if (empty? rows)
       [:p none ]

       ;; at least one row.
       ;; TODO: since keys are the same for all rows, pass along rather
       ;; than calling (keys) on each.
       [:table.classes.table-striped
        [:tr
         (concat
          [[:th]]
          (vec (map (fn [key]
                      (th key)) columns)))]
        (tr rows haz-admin 1 columns :td td)])
    
     (if (and (= true haz-admin)
              create-new)
       [:div {:style "float:left;margin-top:0.5em;width:100%"} [:a {:href (:href create-new)}
                                               (:link-text create-new)]]))))
  
(defn tr [rows haz-admin i columns & {:keys [td]}]
  (if (not (empty? rows))
    (let [row (first rows)
          i (if i i 1)
          td (if td td (fn [row key]
                         (cond true [:td (get row key)])))
          students-per-row (:students row)
          tests-per-row (:tests row)]
      (html [:tr (concat
                  [[:th.num i]]

                  (vec (map (fn [key]
                              (td row key))
                            columns)))] ;; TODO: invariant: pass along to recursive (tr) call.

            (tr (rest rows) haz-admin (+ 1 i) columns :td td)))))


(def hide "") ;; used by callers of (html/table)
(defn default-th [key]
  [:th (string/capitalize (string/join "" (rest (seq (str key)))))]) ;; :foo => "foo" => "Foo"

(defn default-td [row key]
  (let [the-val (get row key)]
    (cond (= java.sql.Timestamp (type the-val))
          [:td [:span {:class "date"}
                (display-time the-val)]]
          (number? the-val)
          [:td.num the-val]
          true
          [:td the-val])))

(defn printfs [fs & filename]
  "print a feature structure to a file. filename will be something easy to derive from the fs."
  (let [filename (if filename (first filename) "foo.html")]  ;; TODO: some conventional default if deriving from fs is too hard.
    (spit filename (static-page (tablize fs) filename))))

(defn plain [expr]
  "simply map expr in a map with one key :plain, whose value is expr.
   workbook/workbookq will format this accordingly."
  {:plain expr})

(defn rows2table [rows & [{cols :cols
                           col-fns :col-fns
                           td-styles :td-styles
                           th-styles :th-styles}]]
  "Take a vector of maps, each of which is a row from a db table, and render it 
in HTML. If :cols is supplied, use it as the vector of column names as keywords.
"
  [:div.rows2table
   (if (empty? rows)
     [:div [:i "None."]]

     ;; else
     [:table {:class "striped padded"}
      ;; top row: column headers
      [:tr
       (map (fn [col]
              [:th
               (if (get th-styles col)
                 {:style (get th-styles col)})
               (string/capitalize
                (string/replace
                (string/replace-first (str col) ":" "")
                "_" " "))
               ])
            (if cols
             cols
             (keys (first rows))))]
      ;; body rows
      (map
       (fn [row]
         [:tr
          (map (fn [col]
                 [:td
                  (if (get td-styles col)
                    {:style (get td-styles col)})
                  (if (get col-fns col)
                    ((get col-fns col) row)
                    (get row col))])
               (if cols
                 cols
                 (keys row)))])
       rows)])])

(defn multipart-to-edn [params]
  (log/trace (str "multipart-to-edn input: " params))
  (let [output
        (zipmap (map #(keyword %)
                     (map #(string/replace % ":" "")
                          (map #(string/replace % "[]" "")
                               (keys params))))
                (vals params))]
    (log/trace (str "multipart-to-edn output: " output))
    output))

(defn banner [segments]
  (if (not (empty? segments))
    (html (if (:href (first segments)) 
            [:a {:href (:href (first segments))}
             (:content (first segments))]
            (:content (first segments)))
          (if (not (empty? (rest segments))) " : ")
          (banner (rest segments)))))

