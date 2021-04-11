(ns ojalgo-clj.core
  (:require [clojure.core.matrix.implementations :as mi]
            [clojure.core.matrix.protocols :as mp])
  (:import [ojalgo-clj.function UnaryFn BinaryFn]
           [org.ojalgo.array Array1D]
           [org.ojalgo.matrix.store MatrixStore$LogicalBuilder Primitive64Store]))



(declare create-vector)
(declare create-matrix)



;; Vector type


(extend-type Array1D



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

  (get-shape [m] [(.size ^Array1D m)])

  (is-scalar? [m] false)

  (is-vector? [m] true)

  (dimension-count [m dimension-number]
    (case (int dimension-number)
      0 (.size ^Array1D m)
      (throw (Exception. "Dimension not supported."))))



  mp/PIndexedAccess


  (get-1d [m row] (.get ^Array1D m (long row)))

  (get-2d [m row column])

  (get-nd [m indexes]
    (if (= 1 (count indexes))
      (.get ^Array1D m (long (first indexes)))
      (throw (Exception. "Attempted to get index not consistent with vector type."))))



  mp/PIndexedSetting


  (set-1d [m row v]
    (let [clone (.copy ^Array1D m)]
      (.set clone (long row) (double v))
      clone))

  (set-2d [m row column v])

  (set-nd [m indexes v]
    (if (= 1 (count indexes))
      (let [clone (.copy ^Array1D m)]
        (.set clone (long (first indexes)) (double v))
        clone)
      (throw (Exception. "Attempted to set index not consistent with vector type."))))

  (is-mutable? [m] true)



  mp/PIndexedSettingMutable


  (set-1d! [m row v]
    (.set ^Array1D m 
          (long row) (double v)))

  (set-2d! [m row column v])

  (set-nd! [m indexes v]
    (if (= 1 (count indexes))
      (.set ^Array1D m 
          (long (first indexes)) (double v))
      (throw (Exception. "Attempted to set index not consistent with vector type."))))



  mp/PMatrixCloning


  (clone [m] (.copy ^Array1D m))



  mp/PSliceView


  (get-major-slice-view [m i]
    (nth (seq m) i))



  mp/PFunctionalOperations


  (element-seq [m]
    (.copy m))

  (element-map
    ([m f]
     (let [m-new (.copy m)]
       (mp/element-map! m-new f)
       m-new))
    ([m f a]
     (let [m-new (.copy m)]
       (mp/element-map! m-new f a)
       m-new))
    ([m f a more]
     (let [m-new (.copy m)]
       (mp/element-map! m-new f a more)
       m-new)))

  (element-map!
    ([m f]
     (.modifyAll m (UnaryFn. f)))
    ([m f a]
     (.modifyMatching m 
                      (BinaryFn. f) 
                      (if (instance? Array1D a) 
                        a (create-vector (mp/convert-to-nested-vectors a)))))
    ([m f a more]
     (.modifyMatching m 
                      (BinaryFn. f) 
                      (if (instance? Array1D a) 
                        a 
                        (create-vector (mp/convert-to-nested-vectors a))))
     (run! (.modifyMatching m 
                            (BinaryFn. f) 
                            (if (instance? Array1D more) 
                              more (create-vector (mp/convert-to-nested-vectors more)))) 
           more)))

  (element-reduce
    ([m f]
    (reduce f (mp/element-seq m)))
    ([m f init]
     (reduce f (double init) (mp/element-seq m))))



  mp/PNumerical


  (numerical? [m] true)
)

(defn create-vector [data]
  (when (and (isa? (class data) java.util.Collection)
             (every? number? data))
    (->> data 
         double-array
         (.copy Array1D/PRIMITIVE64))))



;; Matrix type


(extend-type Primitive64Store



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
    [(.countRows ^Primitive64Store m) 
     (.countColumns ^Primitive64Store m)])

  (is-scalar? [m] false)

  (is-vector? [m] false)

  (dimension-count [m dimension-number]
    (case (int dimension-number)
      0 (.countRows ^Primitive64Store m)
      1 (.countColumns ^Primitive64Store m)
      (throw (Exception. "Dimension not supported."))))



  mp/PIndexedAccess


  (get-1d [m row])

  (get-2d [m row column] 
    (.get ^Primitive64Store m (long row) (long column)))

  (get-nd [m indexes]
    (if (= 2 (count indexes))
      (.get ^Primitive64Store m 
            (long (first indexes)) (long (second indexes)))
      (throw (Exception. "Attempted to get index not consistent with matrix type."))))



  mp/PIndexedSetting


  (set-1d [m row v])

  (set-2d [m row column v]
          (let [clone (.copy ^Primitive64Store m)]
            (.set clone (long row) (long column) (double v))
            clone))

  (set-nd [m indexes v]
    (if (= 2 (count indexes))
      (let [clone (.copy ^Primitive64Store m)]
        (.set clone (long (first indexes)) (long (second indexes)) (double v))
        clone)
      (throw (Exception. "Attempted to set index not consistent with matrix type."))))

  (is-mutable? [m] true)



  mp/PIndexedSettingMutable


  (set-1d! [m row v])

  (set-2d! [m row column v] 
    (.set ^Primitive64Store m 
          (long row) (long column) (double v)))

  (set-nd! [m indexes v]
    (if (= 2 (count indexes))
      (.set ^Primitive64Store m 
            (long (first indexes)) (long (second indexes)) (double v))
      (throw (Exception. "Attempted to set index not consistent with matrix type."))))



  mp/PMatrixCloning


  (clone [m] (.copy ^Primitive64Store m))



  mp/PSliceView


  (get-major-slice-view [m i]
    (nth (seq m) i))



  mp/PFunctionalOperations


  (element-seq [m]
    (-> m
        (.transpose)
        (.copy)
        ^Array1D (.asList)))

  (element-map
    ([m f]
     (let [m-new (.copy m)]
       (mp/element-map! m-new f)
       m-new))
    ([m f a]
     (let [m-new (.copy m)]
       (mp/element-map! m-new f a)
       m-new))
    ([m f a more]
     (let [m-new (.copy m)]
       (mp/element-map! m-new f a more)
       m-new)))

  (element-map! 
    ([m f]
     (.modifyAll m (UnaryFn. f)))
    ([m f a]
     (-> (.operateOnMatching m 
                             (BinaryFn. f) 
                             (if (= (mp/get-shape m) (mp/get-shape a)) 
                               (if (instance? Primitive64Store a) 
                                 a 
                                 (create-matrix (mp/convert-to-nested-vectors a)))
                               (-> (mp/broadcast-compatible m a)
                                   second
                                   mp/convert-to-nested-vectors
                                   create-matrix)))
         (.supplyTo m)))
    ([m f a more]
     (loop [tmp (.operateOnMatching m 
                                    (BinaryFn. f) 
                                    (if (= (mp/get-shape m) (mp/get-shape a)) 
                                      (if (instance? Primitive64Store a) 
                                        a 
                                        (create-matrix (mp/convert-to-nested-vectors a)))
                                      (-> (mp/broadcast-compatible m a)
                                          second
                                          mp/convert-to-nested-vectors
                                          create-matrix)))
            b more]
       (if (empty? b)
         (.supplyTo m tmp)
         (recur (.operateOnMatching m 
                                    (BinaryFn. f) 
                                    (if (= (mp/get-shape m) (mp/get-shape (first b))) 
                                      (if (instance? Primitive64Store (first b)) 
                                        (first b) 
                                        (create-matrix (mp/convert-to-nested-vectors (first b))))
                                      (-> (mp/broadcast-compatible m (first b))
                                          second
                                          mp/convert-to-nested-vectors
                                          create-matrix)))
                (next b))))))

  (element-reduce
    ([m f]
     (reduce f (mp/element-seq m)))
    ([m f init]
     (reduce f (double init) (mp/element-seq m))))
  



  mp/PNumerical


  (numerical? [m] true)



  mp/PMatrixMultiplyMutable


  (matrix-multiply! [m a]
    (if (instance? Primitive64Store a)
      (.multiply m a m)
      (let [a (create-matrix (mp/convert-to-nested-vectors a))]
        (.multiply m a m))))

  (element-multiply! [m a]
    (mp/element-map! m clojure.core/* a))



  mp/PAssignment


  (assign!
    [m source]
    (let [[m a] (mp/broadcast-compatible m source)]
      (.supplyTo (create-matrix (mp/convert-to-nested-vectors a)) 
                 m)))
  (assign-array!
    ([m arr]
     (.fillMatching m (create-vector (into [] arr)))
     (.supplyTo (.copy (.transpose m)) m))
    ([m arr start length]
     (let [arr (subvec arr start (last (take (inc length) (iterate inc start))))]
       (.fillMatching m (create-vector (into [] arr)))
       (.supplyTo (.copy (.transpose m)) m))))
)

(defn create-matrix [data]
  (when (and (isa? (class data) java.util.Collection)
             (every? coll? data)
             (map  #(every? number? %) data))
    (->> data 
         (map double-array)
         into-array
         (.rows Primitive64Store/FACTORY))))



;; Printing methods

(defmethod print-dup Array1D 
  ([^Array1D m ^java.io.Writer w]
   (.write w ^java.lang.String (str "#ojalgo-clj.core/vector " 
                                    (.toString m)))))

(defmethod print-method Array1D 
  ([^Array1D m ^java.io.Writer w]
   (.write w ^java.lang.String (str "#ojalgo-clj.core/vector " 
                                    (.toString m)))))

(defmethod print-dup Primitive64Store 
  ([^Primitive64Store m ^java.io.Writer w]
   (.write w ^java.lang.String (->> (.toString m)
                                    (re-find #"(?is)< \d+ x \d+ >.*" )
                                    (str "#ojalgo-clj.core/matrix ")))))

(defmethod print-method Primitive64Store 
  ([^Primitive64Store m ^java.io.Writer w]
   (.write w ^java.lang.String (->> (.toString m)
                                    (re-find #"(?is)< \d+ x \d+ >.*" )
                                    (str "#ojalgo-clj.core/matrix ")))))



;; Registration


(mi/register-implementation (create-matrix [[0 0] [0 0]]))



