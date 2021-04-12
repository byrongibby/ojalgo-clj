(ns ojalgo-clj.matrix-api-test
  (:require [clojure.core.matrix :as mat]
            [clojure.test :refer :all]
            [ojalgo-clj.core :refer :all]))



(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(mat/set-current-implementation :ojalgo)


(deftest optional-protocols-test



  (testing "Testing matrix operations"
    (let [m (mat/matrix [[1 2] [3 4]])]
      (is (== 5.0 (mat/trace m)))
      (is (== -2.0 (mat/det m)))
      (is (mat/equals (mat/matrix [[-2 1] [1.5 -0.5]]) 
                      (mat/inverse m)  
                      1E-10)))
    (let [m (mat/matrix [[1 4 8] [2 5 8] [2 6 9]])]
      (is (== 15.0 (mat/trace m)))
      (is (== 5.0 (mat/det m)))
      (is (mat/equals (mat/matrix [[-0.6  2.4 -1.6] 
                                   [-0.4 -1.4  1.6] 
                                   [ 0.4  0.4 -0.6]]) 
                      (mat/inverse m) 
                      1E-10))))



  (testing "Testing linear algebra"
    )
) 



