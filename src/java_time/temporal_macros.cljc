(ns java-time.temporal-macros
  (:require [java-time.core :as jt.c])
  #?(:clj
     (:import [java.time.temporal Temporal TemporalAccessor ValueRange
               TemporalField TemporalUnit TemporalAmount ChronoField IsoFields]
       [java.time.format DateTimeFormatter]
       [java.time.chrono Chronology]
       [java.time DateTimeException Clock
        Period Duration MonthDay DayOfWeek Month Year
        ZoneOffset Instant])))

(def writable-range-property-fns
  {:with-min-value (fn [p] (jt.c/with-value p (jt.c/min-value p)))
   :with-largest-min-value (fn [p] (jt.c/with-value p (jt.c/largest-min-value p)))
   :with-smallest-max-value (fn [p] (jt.c/with-value p (jt.c/smallest-max-value p)))
   :with-max-value (fn [p] (jt.c/with-value p (jt.c/max-value p)))})

(defmacro value-property [java-type range-field &
                          {:keys [with-value-fn-sym get-value-fn-sym]
                           :or {with-value-fn-sym 'of
                                get-value-fn-sym 'getValue}}]
  (let [java-type-arg (with-meta (gensym) {:tag java-type})]
    `(do
       (extend-type ~java-type
         jt.c/ReadableProperty
         (jt.c/value [d#]
           (. d# ~get-value-fn-sym))

         jt.c/WritableProperty
         (jt.c/with-value [_# v#]
           (. ~java-type ~with-value-fn-sym v#)))

       (extend-type ~java-type
         jt.c/ReadableRangeProperty
         (jt.c/min-value  [~'p] (.getMinimum ^ValueRange (range ~'p)))
         (jt.c/largest-min-value [~'p] (.getLargestMinimum ^ValueRange (range ~'p)))
          (jt.c/smallest-max-value [~'p] (.getSmallestMaximum ^ValueRange (range ~'p)))
          (jt.c/max-value [~'p] (.getMaximum ^ValueRange (range ~'p)))
         (jt.c/range [~java-type-arg]
           (.range ~java-type-arg ~range-field))

         jt.c/WritableRangeProperty
          (jt.c/with-min-value [~'p] (jt.c/with-value ~'p (jt.c/min-value ~'p)))
          (jt.c/with-largest-min-value [~'p] (jt.c/with-value ~'p (jt.c/largest-min-value ~'p)))
          (jt.c/with-smallest-max-value [~'p] (jt.c/with-value ~'p (jt.c/smallest-max-value ~'p)))
          (jt.c/with-max-value [~'p] (jt.c/with-value ~'p (jt.c/max-value ~'p)))))))



(defmacro field-property [java-type has-range?]
  (let [java-type-arg (with-meta (gensym) {:tag java-type})]
    `(do
       (extend-type ~java-type
         jt.c/ReadableProperty
          (value [~java-type-arg]
                   (get-long-property-value (.o ~java-type-arg)
                     (.field ~java-type-arg))))

       ~(when has-range?
          `(extend-type ~java-type
             jt.c/ReadableRangeProperty
             (jt.c/min-value  [~'p] (.getMinimum ^ValueRange (range ~'p)))
             (jt.c/largest-min-value [~'p] (.getLargestMinimum ^ValueRange (range ~'p)))
             (jt.c/smallest-max-value [~'p] (.getSmallestMaximum ^ValueRange (range ~'p)))
             (jt.c/max-value [~'p] (.getMaximum ^ValueRange (range ~'p)))
             (jt.c/range [~java-type-arg]
               (get-field-property-range (.o ~java-type-arg)
                 (.field ~java-type-arg)))
             

             jt.c/WritableRangeProperty
             writable-range-property-fns)))))
