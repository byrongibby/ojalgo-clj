(ns ojalgo-clj.matrix-api-test
  (:require [clojure.core.matrix :as mat]
            [clojure.core.matrix.linear :as lin]
            [clojure.test :refer :all]
            [ojalgo-clj.core :refer :all]))



(compile 'ojalgo-clj.matrix-api)

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(mat/set-current-implementation :ojalgo)



(deftest optional-protocols-test



  (testing "Testing type information"
    (let [m (mat/matrix [[1 2] [3 4]])]
      (is (isa? (mat/element-type m) Double/TYPE))))



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



  (testing "Testing matrix transpose"
    (let [m (mat/matrix [[1 2] [3 4]])]
      (is (mat/equals (mat/matrix [[1 3] 
                                   [2 4]]) 
                      (mat/transpose m))))
    (let [m (mat/matrix [[1 2 3 4] [5 6 7 8] [9 10 11 12]])]
      (is (mat/equals (mat/matrix [[1 5 9] 
                                   [2 6 10]
                                   [3 7 11]
                                   [4 8 12]]) 
                      (mat/transpose m)))))



  (testing "Testing QR decomposition"
    (let [qr (lin/qr (mat/matrix [[1 2] [3 4]]))
          Q (mat/matrix [[-0.3162278 -0.9486833] 
                         [-0.9486833  0.3162278]])
          R (mat/matrix [[-3.162278 -4.4271887] 
                         [ 0.000000 -0.6324555]])]
      (is (mat/equals (:Q qr) Q 1E-6))
      (is (mat/equals (:R qr) R 1E-6))))



  (testing "Testing Cholesky decomposition"
    (let [chol (lin/cholesky (mat/matrix [[5 1] [1 3]]))
          L (mat/matrix [[2.2360680 0.0000000] 
                         [0.4472136 1.6733201]])
          L* (mat/matrix [[2.2360680 0.4472136] 
                         [0.0000000 1.6733201]])]
      (is (mat/equals (:L chol) L 1E-6))
      (is (mat/equals (:L* chol) L* 1E-6))))



  (testing "Testing LU decomposition"
    (let [lu (lin/lu (mat/matrix [[5 1] [1 3]]))
          L (mat/matrix [[1.0 0.0] 
                         [0.2 1.0]])
          U (mat/matrix [[5.0 1.0] 
                         [0.0 2.8]])]
      (is (mat/equals (:L lu) L 1E-6))
      (is (mat/equals (:U lu) U 1E-6))))



  (comment
    ;; Because SVD decomposition is not unique I think it would
    ;; be better to test whether X = U S V*, but I need to implement
    ;; higher level array indexing first, so that I can select the
    ;; necessary sub-matrix to test the multiplication (should also
    ;; test whether UU'=I and VV'=I)
  (testing "Testing SVD decomposition"
    (let [svd (lin/svd (mat/matrix [[1 5 9] 
                                    [2 6 10]
                                    [3 7 11]
                                    [4 8 12]]))
          U (mat/matrix [[-0.5045331 -0.76077568  0.4082483] 
                         [-0.5745157 -0.05714052 -0.8164966]
                         [-0.6444983  0.64649464  0.4082483]])
          S (mat/matrix [2.546241E+01 1.290662E+00 1.716561E-15])
          V* (mat/matrix [[-0.1408767  0.82471435 -0.4991558] 
                          [-0.3439463  0.42626394  0.4974744]
                          [-0.5470159  0.02781353  0.5025186]
                          [-0.7500855 -0.37063688 -0.5008372]])]
      (is (mat/equals (:U svd) U 1E-6))
      (is (mat/equals (:S svd) S 1E-6))
      (is (mat/equals (:V* svd) V* 1E-6)))))

  (testing "Testing the solving of a linear system of equations"
    (let [a (mat/matrix [[1 5 6] 
                         [4 6 8]
                         [3 2 2]])
          b (mat/matrix [[1] [2] [3]])
          c [[1 0 0]
             [0 1 0]
             [0 0 1]]]
      (mat/equals (lin/solve a b) (mat/matrix [[0.750] [2.000] [-1.625]]) 1E-6)
      (mat/equals (lin/solve a c) (mat/matrix [[-0.250  0.1250  0.250] 
                                               [ 1.000 -1.0000  1.000]
                                               [-0.625  0.8125 -0.875]]) 1E-6)))
)



