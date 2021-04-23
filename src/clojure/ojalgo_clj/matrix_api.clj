(ns ojalgo-clj.matrix-api
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.utils :refer [error]])
  (:import [ojalgo_clj.core Matrix]
           [org.ojalgo.matrix.decomposition Cholesky LDL LDU LU QR SingularValue]
           [org.ojalgo.matrix.task DeterminantTask InverterTask SolverTask]
           [org.ojalgo.matrix.store DiagonalStore LowerTriangularStore UpperTriangularStore Primitive64Store TransposedStore]))


(set! *warn-on-reflection* true)



;; ============================================================
;; Optional protocols



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
      (.calculateDeterminant (.make DeterminantTask/PRIMITIVE (.-p64store m))
                             (.-p64store m))
      (error "Attempted to calculate the determinant of a non-square matrix.")))

  (inverse [m]
    (if (apply = (mp/get-shape m))
      (try 
        (-> (.invert (.make InverterTask/PRIMITIVE (.-p64store m))
                     (.-p64store m))
            (Matrix.))
        (catch Exception _ nil))
      (error "Attempted to calculate the inverse of a non-square matrix."))))

(extend-protocol mp/PTranspose
  Matrix
  (transpose [m]
    (let [m' (.makeZero Primitive64Store/FACTORY 
                        (.countColumns ^Primitive64Store (.-p64store m))
                        (.countRows ^Primitive64Store (.-p64store m)))] 
      (.supplyTo (.transpose ^Primitive64Store (.-p64store m)) ^Primitive64Store m')
      (Matrix. m'))))



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
    (let [{:keys [return compact] 
           :or {return [:Q :R] compact false}} options
          qr-decomp (.make QR/PRIMITIVE true)]
      (when (.decompose qr-decomp (.-p64store m))
        (select-keys 
          {:Q (Matrix. (.getQ qr-decomp))
           :R (when (some #(= :R %) return)
                (let [R (.makeZero Primitive64Store/FACTORY 
                                   (if compact
                                     (min (.countRows ^Primitive64Store (.-p64store m))
                                          (.countColumns ^Primitive64Store (.-p64store m)))
                                     (.countRows ^Primitive64Store (.-p64store m)))
                                   (.countColumns ^Primitive64Store (.-p64store m)))]
                  (.supplyTo (.getR qr-decomp) ^Primitive64Store R)
                  (Matrix. R)))}
          return)))))

(extend-protocol mp/PCholeskyDecomposition
  Matrix
  (cholesky [m options]
    (let [{:keys [return] :or {return [:L :L*]}} options
          cholesky-decomp (.make Cholesky/PRIMITIVE
                                 (.countRows ^Primitive64Store (.-p64store m))
                                 (.countColumns ^Primitive64Store (.-p64store m)))
          L (.makeZero Primitive64Store/FACTORY 
                       (.countRows ^Primitive64Store (.-p64store m)) 
                       (.countColumns ^Primitive64Store (.-p64store m)))]
      (when (.decompose cholesky-decomp (.-p64store m))
        (.supplyTo ^LowerTriangularStore (.getL cholesky-decomp) ^Primitive64Store L)
        (select-keys 
          {:L (Matrix. L)
           :L* (when (some #(= :L* %) return)
                 (let [L* (.makeZero Primitive64Store/FACTORY 
                                     (.countRows ^Primitive64Store (.-p64store m)) 
                                     (.countColumns ^Primitive64Store (.-p64store m)))] 
                   (.supplyTo (.transpose ^Primitive64Store L) ^Primitive64Store L*)
                   (Matrix. L*)))}
          return)))))

(extend-protocol mp/PLUDecomposition
  Matrix
  (lu [m options]
    (let [{:keys [return] :or {return [:L :U :P]}} options
          lu-decomp (.make LU/PRIMITIVE
                           (.countRows ^Primitive64Store (.-p64store m))
                           (.countColumns ^Primitive64Store (.-p64store m)))
          L (.makeZero Primitive64Store/FACTORY 
                       (.countRows ^Primitive64Store (.-p64store m)) 
                       (.countColumns ^Primitive64Store (.-p64store m)))]
      (when (.decompose lu-decomp (.-p64store m))
        (.supplyTo ^LowerTriangularStore (.getL lu-decomp) ^Primitive64Store L)
        (select-keys 
          {:L (Matrix. L)
           :U (when (some #(= :U %) return)
                (let [U (.makeZero Primitive64Store/FACTORY 
                                   (.countRows ^Primitive64Store (.-p64store m)) 
                                   (.countColumns ^Primitive64Store (.-p64store m)))] 
                  (.supplyTo ^UpperTriangularStore (.getU lu-decomp) ^Primitive64Store U)
                  (Matrix. U)))
           :P (when (some #(= :P %) return)
                (-> (.makeEye Primitive64Store/FACTORY 
                              (.countRows ^Primitive64Store (.-p64store m)) 
                              (.countColumns ^Primitive64Store (.-p64store m)))
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
        (let [B (if (instance? Matrix b) b (mp/construct-matrix a (mp/convert-to-nested-vectors b)))]
          (Matrix. (.solve (.make SolverTask/PRIMITIVE (.-p64store a) (.-p64store B))
                           (.-p64store a)
                           (.-p64store B))))
        (catch Exception _ nil))))



;; ============================================================
;; Dataset protocols



;; ============================================================
;; Utility functions



(comment
  (require '[ojalgo-clj.core :refer [create-matrix]])
  (require '[clojure.core.matrix.protocols :as mp]))



