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



