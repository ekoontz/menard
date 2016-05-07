(ns babel.italiano.morphology.articles)
;; TODO: remove this file: not being used.
;; TODO: unify with (babel.italiano.morphology/conjugate-italian-prep)
(def l-apostrophe
  {
   #"^dell$"
   {:replace-with "del"
    :unify-with :top}
   #"^dell$"
   {:replace-with "della"
    :unify-with :top}

   #"^l$"
   {:replace-with "le"
    :unify-with :top}

   #"^l$"
   {:replace-with "la"
    :unify-with :top}

   #"^l$"
   {:replace-with "lo"
    :unify-with :top}

   #"^l$"
   {:replace-with "il"
    :unify-with :top}
   
   #"^gli$"
   {:replace-with "i"
    :unify-with :top}

   }
  )
