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
  (require '[clojure.core.matrix.implementations :as imp])

  (def im (mat/clone (imp/get-canonical-object m)))
  (def ik (imp/get-implementation-key im))

  (defn create-dimensioned
    "Create a test nested vector array with the specified number of dims. will have 2^dims numeric elements"
    ([dims]
     (create-dimensioned dims 1))
    ([dims start]
     (cond
       (<= dims 0) start
       :else (vector (create-dimensioned (dec dims) start)
                     (create-dimensioned (dec dims) (+ start (bit-shift-left 1 (dec dims))))))))

  (defn create-supported-matrices
    "Creates a set of vector matrices of supported dimensionalities from 1 to 4"
    ([m]
     (map
       create-dimensioned
       (filter #(mat/supports-dimensionality? m %)
               (range 1 5)))))

  (def x (create-supported-matrices im))
  (def vm (second x))

  (let [m (mat/coerce im vm)
        len (mat/ecount m)
        vs (range 1 (inc len))]
    (is (every? true? (map == vs (mat/eseq m)))))



  (def m (mat/matrix im [[1 2] [3 4]]))

  (defn mutable-equivalent?
    "Returns true if mutable-fn? is the in-place equivalent of immutable-fn? when applied to m"
    [m mutable-fn immutable-fn]
    (or
      (not (mat/mutable? m))
      (let [clonem (mat/clone m)]
        (println (mutable-fn clonem))
        (println (immutable-fn m))
        (mutable-fn clonem)
        (mat/equals clonem (immutable-fn m)))))

  (is (mutable-equivalent? m #(mat/mul! % 2) #(mat/mul % 2)))

  *e

  (defprotocol PMatrixMultiplyMutable
    "Protocol to support mutable matrix multiplication on an arbitrary matrix, vector or scalar"
    (matrix-multiply! [m a])
    (element-multiply! [m a]))
)
