(ns babel.stringutils
  (:refer-clojure :exclude [get-in]))

(require '[clojure.string :as string])
(require '[dag-unify.core :refer [get-in]])

(defn replace-from-list [regexp-list target]
  "Apply the first regexp pair (from=>to) from regexp-list to target;
   if this regexp changes target, return changed string,
   otherwise, try next regexp."
  (if (> (count regexp-list) 0)
    (let [regexp-pair (first regexp-list)
          regexp-from (first regexp-pair)
          regexp-to (second regexp-pair)
          result (string/replace regexp-from regexp-to target)]
      (if (= result target)
        (replace-from-list (rest regexp-list) target)
        result))
    target))

(defn show-as-tree [structure language-feature]
  (cond
    (and (map? structure)
         (or (get-in structure [:a])
             (get-in structure [:b])))
    (str "[" (get-in structure [:rule]) " "
         (show-as-tree (get-in structure [:a]) language-feature)
         " "
         (show-as-tree (get-in structure [:b]) language-feature)
         "]")

    (and (map? structure)
         (get-in structure [language-feature])
         (empty? (get-in structure [language-feature])))
    (str "'" "--" "'")

    (and (map? structure)
         (get-in structure [language-feature]))
    (str "'" (get-in structure [language-feature]) "'")

    true
    (str "??:" structure)))
