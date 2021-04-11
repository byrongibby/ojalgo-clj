(ns ojalgo-clj.matrix-store
  (:gen-class 
    :name ojalgo-clj.matrix-store.Primitive64
    :extends org.ojalgo.matrix.store.Primitive64Store
    :implements [clojure.lang.ISeq]
    :methods [[seq [] clojure.lang.ArraySeq]
              [first [] clojure.lang.ArraySeq]
              [next [] clojure.lang.ArraySeq]
              [cons [org.ojalgo.array.Array1D] clojure.lang.ArraySeq]])
  (:import [org.ojalgo.array Array1D]
           [org.ojalgo.matrix.store MatrixStore$LogicalBuilder]))



(defn -seq [this]
  (for [row-index (range (.countRows this))]
    (-> (.logical this)
        ^MatrixStore$LogicalBuilder (.row (int-array [row-index]))
        (.copy)
        ^Array1D (.asList))))

(defn -first [this]
  (first (-seq this)))

(defn -next [this]
  (next (-seq this)))

(defn -cons [this item]
  (cons item (-seq this)))






