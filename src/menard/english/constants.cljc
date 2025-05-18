(ns menard.english.constants)

(def emoji-identities ;; used in one grammar rule: pronoun+emoji.
  (let [gender (atom :top)
        number (atom :top)
        context (atom :top)
        case (atom :top)
        sem (atom {:ref {:context context}})]
    {:case case
     :sem sem
     :head {:agr {:gender gender
                  :number number}
            :case case
            :sem sem
            :show-notes? false} ;; we'll get the notes from the complement of this rule, which is an emoji/are emojis.
     :comp {:agr {:gender gender
                  :number number}
            :sem {:ref {:context context}}}}))
