(ns ojalgo-clj.core
  (:require [clojure.core.matrix.implementations :as mi]
            [clojure.core.matrix.protocols :as mp])
  (:import [org.ojalgo.array Array1D]
           [org.ojalgo.matrix.store MatrixStore$LogicalBuilder Primitive64Store]))



(declare create-vector)
(declare create-matrix)



;; Vector type


(deftype Vector [^Array1D array1d]



  Object


  (toString [this]
    (->> (.toString (.-array1d this))
         (str "#"  `Vector " " )))



  clojure.lang.ISeq


  (seq [^Vector m]
    (seq (.-array1d m)))

  (first [^Vector this]
    (first (seq this)))

  (next [^Vector this]
    (next (seq this)))

  (cons [item this]
    (cons item (seq this)))



  mp/PImplementation


  (implementation-key [m] :ojalgo)

  (meta-info [m] 
    {:doc "Clojure core.matrix implementation of oj! Algorithm"})

  (construct-matrix [m data]
    (case (mp/dimensionality data)
      1 (create-vector data)
      2 (create-matrix data)
      nil))

  (new-vector [m length]
    (create-vector (repeat length 0)))

  (new-matrix [m rows columns]
    (create-matrix (repeat rows (repeat columns 0))))

  (new-matrix-nd [m shape]
    (case (count shape)
      1 (create-vector (repeat (first shape) 0))
      2 (create-matrix (repeat (first shape) (repeat (second shape) 0)))
      nil))

  (supports-dimensionality? [m dimensions]
    (case dimensions
      1 true
      2 true
      nil))



  mp/PDimensionInfo


  (dimensionality [m] 1)

  (get-shape [m] [(.size ^Array1D (.-array1d m))])

  (is-scalar? [m] false)

  (is-vector? [m] true)

  (dimension-count [m dimension-number]
    (case (int dimension-number)
      0 (.size ^Array1D (.-array1d m))
      (throw (Exception. "Dimension not supported."))))



  mp/PIndexedAccess


  (get-1d [m row] (.get ^Array1D (.-array1d m) (long row)))

  (get-2d [m row column])

  (get-nd [m indexes]
    (if (= 1 (count indexes))
      (.get ^Array1D (.-array1d m) (long (first indexes)))
      (throw (Exception. "Attempted to get index not consistent with vector type."))))



  mp/PIndexedSetting


  (set-1d [m row v]
    (let [clone (.copy ^Array1D (.-array1d m))]
      (.set clone (long row) (double v))
      (Vector. clone)))

  (set-2d [m row column v])

  (set-nd [m indexes v]
    (if (= 1 (count indexes))
      (let [clone (.copy ^Array1D (.-array1d m))]
        (.set clone (long (first indexes)) (double v))
        (Vector. clone))
      (throw (Exception. "Attempted to set index not consistent with vector type."))))

  (is-mutable? [m] true)



  mp/PIndexedSettingMutable


  (set-1d! [m row v]
    (.set ^Array1D (.-array1d m) 
          (long row) (double v)))

  (set-2d! [m row column v])

  (set-nd! [m indexes v]
    (if (= 1 (count indexes))
      (.set ^Array1D (.-array1d m) 
          (long (first indexes)) (double v))
      (throw (Exception. "Attempted to set index not consistent with vector type."))))



  mp/PMatrixCloning


  (clone [m] (Vector. (.copy ^Array1D (.-array1d m))))
)

(defn create-vector [data]
  (when (and (isa? (class data) java.util.Collection)
             (every? number? data))
    (->> data 
         double-array
         (.copy Array1D/PRIMITIVE64)
         (Vector.))))



;; Matrix type


(deftype Matrix [^Primitive64Store p64store]



  Object


  (toString [this]
    (->> (.toString (.-p64store this))
         (re-find #"(?is)< \d+ x \d+ >.*" )
         (str "#"  `Matrix " " )))



  clojure.lang.ISeq


  (seq [^Matrix m]
    (for [row-index (range (.countRows ^Primitive64Store (.-p64store m)))]
      (-> (.logical ^Primitive64Store (.-p64store m))
          ^MatrixStore$LogicalBuilder (.row (int-array [row-index]))
          (.copy)
          (.asList)
          ^Vector (create-vector))))

  (first [^Matrix this]
    (first (seq this)))

  (next [^Matrix this]
    (next (seq this)))

  (cons [item this]
    (cons item (seq this)))



  mp/PImplementation


  (implementation-key [m] :ojalgo)

  (meta-info [m] 
    {:doc "Clojure core.matrix implementation of oj! Algorithm"})

  (construct-matrix [m data]
    (case (mp/dimensionality data)
      1 (create-vector data)
      2 (create-matrix data)
      nil))

  (new-vector [m length]
    (create-vector (repeat length 0)))

  (new-matrix [m rows columns]
    (create-matrix (repeat rows (repeat columns 0))))

  (new-matrix-nd [m shape]
    (case (count shape)
      1 (create-vector (repeat (first shape) 0))
      2 (create-matrix (repeat (first shape) (repeat (second shape) 0)))
      nil))

  (supports-dimensionality? [m dimensions]
    (case dimensions
      1 true
      2 true
      nil))



  mp/PDimensionInfo


  (dimensionality [m] 2)

  (get-shape [m] 
    [(.countRows ^Primitive64Store (.-p64store m)) 
     (.countColumns ^Primitive64Store (.-p64store m))])

  (is-scalar? [m] false)

  (is-vector? [m] false)

  (dimension-count [m dimension-number]
    (case (int dimension-number)
      0 (.countRows ^Primitive64Store (.-p64store m))
      1 (.countColumns ^Primitive64Store (.-p64store m))
      (throw (Exception. "Dimension not supported."))))



  mp/PIndexedAccess


  (get-1d [m row])

  (get-2d [m row column] 
    (.get ^Primitive64Store (.-p64store m) (long row) (long column)))

  (get-nd [m indexes]
    (if (= 2 (count indexes))
      (.get ^Primitive64Store (.-p64store m) 
            (long (first indexes)) (long (second indexes)))
      (throw (Exception. "Attempted to get index not consistent with matrix type."))))



  mp/PIndexedSetting


  (set-1d [m row v])

  (set-2d [m row column v]
          (let [clone (.copy ^Primitive64Store (.-p64store m))]
            (.set clone (long row) (long column) (double v))
            (Matrix. clone)))

  (set-nd [m indexes v]
    (if (= 2 (count indexes))
      (let [clone (.copy ^Primitive64Store (.-p64store m))]
        (.set clone (long (first indexes)) (long (second indexes)) (double v))
        (Matrix. clone))
      (throw (Exception. "Attempted to set index not consistent with matrix type."))))

  (is-mutable? [m] true)



  mp/PIndexedSettingMutable


  (set-1d! [m row v])

  (set-2d! [m row column v] 
    (.set ^Primitive64Store (.-p64store m) 
          (long row) (long column) (double v)))

  (set-nd! [m indexes v]
    (if (= 2 (count indexes))
      (.set ^Primitive64Store (.-p64store m) 
            (long (first indexes)) (long (second indexes)) (double v))
      (throw (Exception. "Attempted to set index not consistent with matrix type."))))



  mp/PMatrixCloning


  (clone [m] (Matrix. (.copy ^Primitive64Store (.-p64store m))))
)

(defn create-matrix [data]
  (when (and (isa? (class data) java.util.Collection)
             (every? coll? data)
             (map  #(every? number? %) data))
    (->> data 
         (map double-array)
         into-array
         (.rows Primitive64Store/FACTORY)
         (Matrix.))))



;; Printing methods


(defmethod print-dup ojalgo_clj.core.Vector 
  ([^Vector m ^java.io.Writer w]
   (.write w ^java.lang.String (.toString m))))

(defmethod print-method ojalgo_clj.core.Vector 
  ([^Vector m ^java.io.Writer w]
   (.write w ^java.lang.String (.toString m))))

(defmethod print-dup ojalgo_clj.core.Matrix 
  ([^Matrix m ^java.io.Writer w]
   (.write w ^java.lang.String (.toString m))))

(defmethod print-method ojalgo_clj.core.Matrix 
  ([^Matrix m ^java.io.Writer w]
   (.write w ^java.lang.String (.toString m))))



;; Registration


(mi/register-implementation (create-matrix [[0 0] [0 0]]))



