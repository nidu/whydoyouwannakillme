(ns whydoyouwannakillme.test-runner
  (:require
   [cljs.test :refer-macros [run-tests]]
   [whydoyouwannakillme.core-test]))

(enable-console-print!)

(defn runner []
  (if (cljs.test/successful?
       (run-tests
        'whydoyouwannakillme.core-test))
    0
    1))
