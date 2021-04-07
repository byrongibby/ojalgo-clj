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
  (def m (mat/array [[0 0] [0 0]]))
  (def dims (mat/dimensionality m))
  (def area (repeat dims 0))
  (testing "select should match select-view"
    (when (> (mat/ecount m) 0)
      (let [area (repeat dims 0)]
        (is (mat/e= (apply mat/select m area) 
                    (apply mat/select-view m area))))))

  (nth (mat/array [0 0]) 0)
)
