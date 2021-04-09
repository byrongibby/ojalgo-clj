(ns ojalgo-clj.core-test
  (:require [clojure.core.matrix :as mat]
            [clojure.test :refer :all]
            [clojure.core.matrix.compliance-tester :refer [compliance-test]]
            [ojalgo-clj.core :refer :all]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(mat/set-current-implementation :ojalgo)

(deftest api-compliance-test
  (testing "Testing API compliance"
    (compliance-test (mat/array [[0 0] [0 0]])))) 

(comment 
  (require '[clojure.core.matrix.protocols :as mp])

  (def m (mat/array [[0 0] [-1 -1]]))
  (def a (mat/array [[1 2] [3 4]]))

  (mp/broadcast-compatible m a)

  (mp/is-mutable? m)

  (mp/element-map m clojure.core/- a)
  (mp/element-map! m clojure.core/- a)

  (mp/matrix-sub m a)
  (mp/matrix-sub! m a) ;Why do you this! ????

  *e

)
