(ns ojalgo-clj.matrix-api
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.utils :refer [error]]
            [ojalgo-clj.core :refer [create-matrix]])
  (:import [ojalgo_clj.core Matrix]
           [org.ojalgo.matrix.decomposition Cholesky LDL LDU LU QR SingularValue]
           [org.ojalgo.matrix.task DeterminantTask InverterTask SolverTask]
           [org.ojalgo.matrix.store DiagonalStore LowerTriangularStore UpperTriangularStore Primitive64Store TransposedStore]))


(set! *warn-on-reflection* true)



;; ============================================================
;; Optional protocols


(extend-protocol mp/PTypeInfo
  Matrix
  (element-type [m]
                Double/TYPE))

(extend-protocol mp/PValidateShape
  Matrix
  (validate-shape
    ([m]
     (let [p ^Primitive64Store (.-p64store m)]
     [(.countRows p) 
      (.countColumns p)])) 
    ([m expected-shape]
     (let [p ^Primitive64Store (.-p64store m)
           shape [(.countRows p) 
                  (.countColumns p)]]
       (if (or (not= (first expected-shape) (first shape))
               (not= (second expected-shape) (second shape)))
         (error (str "Matrix does not conform to the expected shape: " expected-shape))
         shape)))))

(comment
(extend-protocol mp/PRowColMatrix
  "Protocol to support construction of row and column matrices from 1D vectors.
   A vector of length N should be converted to a 1xN or Nx1 matrix respectively.
   Should throw an error if the data is not a 1D vector"
  (column-matrix [m data])
  (row-matrix [m data])))



;; ============================================================
;; Array assignment and conversion operations



;; ============================================================
;; Equality operations



;; ============================================================
;; Mathematical operations



(extend-protocol mp/PMatrixOps
  Matrix
  (trace [m]
    (if (apply = (mp/get-shape m))
      (apply + (.sliceDiagonal ^Primitive64Store (.-p64store m) (long 0) (long 0)))
      (error "Attempted to calculate the trace of a non-square matrix.")))
  (determinant [m]
    (if (apply = (mp/get-shape m))
      (let [p (.-p64store m)]
        (.calculateDeterminant (.make DeterminantTask/PRIMITIVE p) p))
      (error "Attempted to calculate the determinant of a non-square matrix.")))
  (inverse [m]
    (if (apply = (mp/get-shape m))
      (try 
        (let [p (.-p64store m)]
          (-> (.invert (.make InverterTask/PRIMITIVE p) p)
              (Matrix.)))
        (catch Exception _ nil))
      (error "Attempted to calculate the inverse of a non-square matrix."))))

(extend-protocol mp/PTranspose
  Matrix
  (transpose [m]
    (let [p ^Primitive64Store (.-p64store m)
          p' ^Primitive64Store (.makeZero Primitive64Store/FACTORY 
                                          (.countColumns p)
                                          (.countRows p))] 
      (.supplyTo (.transpose  p) p')
      (Matrix. p'))))



;; ============================================================
;; Elementary Row Operation Protocols



;; ============================================================
;; Generic values and functions



;; ============================================================
;; Protocols for higher-level array indexing



;; ============================================================
;; Labeled dimensions



;; ============================================================
;; Linear algebra


(extend-protocol mp/PQRDecomposition
  Matrix
  (qr [m options]
    (let [p ^Primitive64Store (.-p64store m)
          {:keys [return compact] 
           :or {return [:Q :R] compact false}} options
          qr-decomp (.make QR/PRIMITIVE true)]
      (when (.decompose qr-decomp p)
        (select-keys 
          {:Q (Matrix. (.getQ qr-decomp))
           :R (when (some #(= :R %) return)
                (let [R (.makeZero Primitive64Store/FACTORY 
                                   (if compact
                                     (min (.countRows p) (.countColumns p))
                                     (.countRows p))
                                   (.countColumns p))]
                  (.supplyTo (.getR qr-decomp) ^Primitive64Store R)
                  (Matrix. R)))}
          return)))))

(extend-protocol mp/PCholeskyDecomposition
  Matrix
  (cholesky [m options]
    (let [p ^Primitive64Store (.-p64store m)
          {:keys [return] :or {return [:L :L*]}} options
          cholesky-decomp (.make Cholesky/PRIMITIVE p)
          L  ^Primitive64Store (.makeZero Primitive64Store/FACTORY 
                                          (.countRows p) 
                                          (.countColumns p))]
      (when (.decompose cholesky-decomp p)
        (.supplyTo ^LowerTriangularStore (.getL cholesky-decomp) L)
        (select-keys 
          {:L (Matrix. L)
           :L* (when (some #(= :L* %) return)
                 (let [L* ^Primitive64Store (.makeZero Primitive64Store/FACTORY 
                                                       (.countRows p) 
                                                       (.countColumns p))] 
                   (.supplyTo (.transpose L) L*)
                   (Matrix. L*)))}
          return)))))

(extend-protocol mp/PLUDecomposition
  Matrix
  (lu [m options]
    (let [p ^Primitive64Store (.-p64store m)
          {:keys [return] :or {return [:L :U :P]}} options
          lu-decomp (.make LU/PRIMITIVE p)
          L  ^Primitive64Store(.makeZero Primitive64Store/FACTORY 
                                         (.countRows p) 
                                         (.countColumns p))]
      (when (.decompose lu-decomp p)
        (.supplyTo ^LowerTriangularStore (.getL lu-decomp) L)
        (select-keys 
          {:L (Matrix. L)
           :U (when (some #(= :U %) return)
                (let [U ^Primitive64Store (.makeZero Primitive64Store/FACTORY 
                                                     (.countRows p) 
                                                     (.countColumns p))] 
                  (.supplyTo ^UpperTriangularStore (.getU lu-decomp) U)
                  (Matrix. U)))
           :P (when (some #(= :P %) return)
                (-> (.makeEye Primitive64Store/FACTORY 
                              (.countRows p) 
                              (.countColumns p))
                    (Matrix.)))}
          return)))))

(comment
  (extend-protocol mp/PSVDDecomposition
    Matrix
    (svd [m options]
      (let [{:keys [return] :or {return [:U :S :V*]}} options
            svd-decomp (.make SingularValue/PRIMITIVE (.-p64store m))]
        (when (.decompose svd-decomp (.-p64store m))
          (cond-> {}
            (some #(= :U %) return) 
            (assoc :U (let [U (.makeZero Primitive64Store/FACTORY 
                                         (.countRows ^Primitive64Store (.-p64store m)) 
                                         (.countRows ^Primitive64Store (.-p64store m)))] 
                        (.supplyTo ^TransposedStore (.getQ1 svd-decomp) ^Primitive64Store U)
                        (Matrix. U)))

            (some #(= :S %) return)
            (assoc :S (.getSingularValues svd-decomp))

            (some #(= :V* %) return)
            (assoc :V* (let [V* (.makeZero Primitive64Store/FACTORY 
                                           (.countColumns ^Primitive64Store (.-p64store m)) 
                                           (.countColumns ^Primitive64Store (.-p64store m)))] 
                         (.supplyTo ^TransposedStore (.getQ2 svd-decomp) ^Primitive64Store V*)
                         (Matrix. V*))))))))
)

(extend-protocol mp/PSolveLinear
    Matrix
    (solve [a b]
      (try 
        (let [p ^Primitive64Store (.-p64store a)
              q ^Primitive64Store (.-p64store (create-matrix b))]
          (Matrix. (.solve (.make SolverTask/PRIMITIVE p q) p q)))
        (catch Exception _ nil))))



;; ============================================================
;; Dataset protocols



;; ============================================================
;; Utility functions



(comment
  (require '[ojalgo-clj.core :refer [create-matrix]])
  (require '[clojure.core.matrix.protocols :as mp]))



