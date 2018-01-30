(ns java-time.util)

(defmacro static-prop [target prop]
  `(goog.object/get ~target (str ~prop)))

(defmacro static-call [target prop & args]
  `((goog.object/get ~target (str ~prop)) ~@args))


