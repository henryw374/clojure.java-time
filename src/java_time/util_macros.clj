(ns java-time.util-macros)

(defmacro static-prop [target prop]
  `(. ~target ~(second prop)))

(defmacro static-call [target prop & args]
  `(. ~target ~(second
                 prop) ~@args))
