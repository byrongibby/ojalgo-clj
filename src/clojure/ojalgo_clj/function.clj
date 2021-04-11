(ns ojalgo-clj.function)

(gen-class 
  :name ojalgo-clj.function.UnaryFn
  :prefix UnaryFn-
  :implements [org.ojalgo.function.UnaryFunction]
  :state state
  :init init
  :constructors {[clojure.lang.IFn] []}
  :methods [^:static [invoke [Double] Double]])

(defn UnaryFn-init [f]
  [[] f])

(defn UnaryFn-invoke [this arg]
  ((.state this) arg))

(gen-class 
  :name ojalgo-clj.function.BinaryFn
  :prefix BinaryFn-
  :implements [org.ojalgo.function.BinaryFunction]
  :state state
  :init init
  :constructors {[clojure.lang.IFn] []}
  :methods [^:static [invoke [Double Double] Double]])

(defn BinaryFn-init [f]
  [[] f])

(defn BinaryFn-invoke [this arg1 arg2]
  ((.state this) arg1 arg2))

(comment
; Reifeid functional interfaces that allow functional-style operations on matrix elements


(defn unary-fn [f]
  (reify org.ojalgo.function.UnaryFunction
    (^double invoke [this ^double arg]
      (f arg))))

(defn binary-fn [f]
  (reify org.ojalgo.function.BinaryFunction
    (^double invoke [this ^double arg1 ^double arg2]
      (f arg1 arg2)))))



