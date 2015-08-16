(ns babel.stringutils)
(require '[clojure.string :as string])

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
  
