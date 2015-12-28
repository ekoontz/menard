(ns babel.runner
  (:require #?(:cljs [doo.runner :refer-macros [doo-tests]])
            [babel.core]))

#?(:cljs (doo-tests 'babel.test.pos))
;;#?(:cljs (doo-tests 'babel.test.en))




