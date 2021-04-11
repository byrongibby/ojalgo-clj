(ns ojalgo-clj.core
  (:require [clojure.core.matrix.implementations :as mi]
            [clojure.core.matrix.protocols :as mp])
  (:import [ojalgo-clj.lambda UnaryFn BinaryFn]
           [org.ojalgo.array Array1D]
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



  mp/PSliceView


  (get-major-slice-view [m i]
    (nth (seq m) i))



  mp/PFunctionalOperations


  (element-seq [m]
    (Vector. (.copy (.-array1d m))))

  (element-map
    [m f]
    (let [m-new (Vector. (.copy (.-array1d m)))]
      (mp/element-map! m-new f)
      m-new))
  (element-map
    [m f a]
    (let [m-new (Vector. (.copy (.-array1d m)))]
      (mp/element-map! m-new f a)
      m-new))
  (element-map
    [m f a more]
    (let [m-new (Vector. (.copy (.-array1d m)))]
      (mp/element-map! m-new f a more)
      m-new))

  (element-map!
    [m f]
    (.modifyAll (.-array1d m) (UnaryFn. f)))
  (element-map!
    [m f a]
    (.modifyMatching (.-array1d m) 
                     (BinaryFn. f) 
                     (.-array1d (if (instance? Array1D a) 
                                  a (create-vector (mp/convert-to-nested-vectors a))))))
  (element-map!
    [m f a more]
    (.modifyMatching (.-array1d m) 
                     (BinaryFn. f) 
                     (.-array1d (if (instance? Array1D a) 
                                  a 
                                  (create-vector (mp/convert-to-nested-vectors a)))))
    (run! (.modifyMatching (.-array1d m) 
                           (BinaryFn. f) 
                           (.-array1d (if (instance? Array1D more) 
                                        more (create-vector (mp/convert-to-nested-vectors more))))) 
          more))

  (element-reduce
    [m f]
    (reduce f (mp/element-seq m)))
  (element-reduce
    [m f init]
    (reduce f (double init) (mp/element-seq m)))



  mp/PNumerical


  (numerical? [m] true)
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
          ^Vector (Vector.))))

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



  mp/PSliceView


  (get-major-slice-view [m i]
    (nth (seq m) i))



  mp/PFunctionalOperations



  (element-seq [m]
    (-> (.-p64store m)
        (.transpose)
        (.copy)
        (.asList)
        (Vector.)))

  (element-map
    [m f]
    (let [m-new (Matrix. (.copy (.-p64store m)))]
      (mp/element-map! m-new f)
      m-new))
  (element-map
    [m f a]
    (let [m-new (Matrix. (.copy (.-p64store m)))]
      (mp/element-map! m-new f a)
      m-new))
  (element-map
    [m f a more]
    (let [m-new (Matrix. (.copy (.-p64store m)))]
      (mp/element-map! m-new f a more)
      m-new))

  (element-map! [m f]
    (.modifyAll (.-p64store m) (UnaryFn. f)))
  (element-map! [m f a]
    (-> (.operateOnMatching (.-p64store m) 
                            (BinaryFn. f) 
                            (.-p64store 
                              (if (= (mp/get-shape m) (mp/get-shape a)) 
                                (if (instance? Primitive64Store a) 
                                  a 
                                  (create-matrix (mp/convert-to-nested-vectors a)))
                                (-> (mp/broadcast-compatible m a)
                                    second
                                    mp/convert-to-nested-vectors
                                    create-matrix))))
          (.supplyTo (.-p64store m))))
  (element-map! [m f a more]
    (loop [tmp (.operateOnMatching (.-p64store m) 
                                   (BinaryFn. f) 
                                   (.-p64store 
                                     (if (= (mp/get-shape m) (mp/get-shape a)) 
                                       (if (instance? Primitive64Store a) 
                                         a 
                                         (create-matrix (mp/convert-to-nested-vectors a)))
                                       (-> (mp/broadcast-compatible m a)
                                           second
                                           mp/convert-to-nested-vectors
                                           create-matrix))))
           b more]
      (if (empty? b)
        (.supplyTo (.-p64store m) tmp)
        (recur (.operateOnMatching (.-p64store m) 
                                   (BinaryFn. f) 
                                   (.-p64store 
                                     (if (= (mp/get-shape m) (mp/get-shape (first b))) 
                                       (if (instance? Primitive64Store (first b)) 
                                         (first b) 
                                         (create-matrix (mp/convert-to-nested-vectors (first b))))
                                       (-> (mp/broadcast-compatible m (first b))
                                           second
                                           mp/convert-to-nested-vectors
                                           create-matrix))))
               (next b)))))

  (element-reduce
    [m f]
    (reduce f (mp/element-seq m)))
  (element-reduce
    [m f init]
    (reduce f (double init) (mp/element-seq m)))



  mp/PNumerical


  (numerical? [m] true)



  mp/PMatrixMultiplyMutable


  (matrix-multiply! [m a]
    (if (instance? Primitive64Store a)
      (.multiply (.-p64store m) (.-p64store a) (.-p64store m))
      (let [a (create-matrix (mp/convert-to-nested-vectors a))]
        (.multiply (.-p64store m) (.-p64store a) (.-p64store m)))))

  (element-multiply! [m a]
    (mp/element-map! m clojure.core/* a))



  mp/PAssignment


  (assign!
    [m source]
    (let [[m a] (mp/broadcast-compatible m source)]
      (.supplyTo (.-p64store (create-matrix (mp/convert-to-nested-vectors a))) 
                 (.-p64store m))))
  (assign-array!
    [m arr]
    (.fillMatching (.-p64store m) (.-array1d (create-vector (into [] arr))))
    (.supplyTo (.copy (.transpose (.-p64store m))) (.-p64store m)))
  (assign-array!
    [m arr start length]
    (let [arr (subvec arr start (last (take (inc length) (iterate inc start))))]
      (.fillMatching (.-p64store m) (.-array1d (create-vector (into [] arr))))
      (.supplyTo (.copy (.transpose (.-p64store m))) (.-p64store m))))
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



