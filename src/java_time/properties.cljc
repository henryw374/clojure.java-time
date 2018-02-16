(ns java-time.properties
  (:require [java-time.core :as jt.c]
            [java-time.util :as jt.u])
  #?(:clj (:import [java.time.temporal
               TemporalField IsoFields ChronoField JulianFields
               TemporalUnit ChronoUnit])))

#?(:cljs
   (do
     (def TemporalField (.. js/JSJoda -TemporalField))
     (def IsoFields (.. js/JSJoda -IsoFields))
     (def ChronoField (.. js/JSJoda -ChronoField))
     (def TemporalUnit (.. js/JSJoda -TemporalUnit))
     (def ChronoUnit (.. js/JSJoda -ChronoUnit))
     ;(def JulianFields (.. js/JSJoda -JulianFields))
     ))

(defn- property->key [p]
  (keyword (jt.u/dashize (str p))))

(defn- ->map [xs]
  (zipmap (->> xs (map property->key))
          xs))

;;;;;;;;; Field/Unit groups

(deftype FieldGroup [group-id field-map]
  jt.c/HasFields
  (fields [_] field-map)
  (field* [_ k] (k field-map))
  Object
  (toString [_] group-id))

(deftype UnitGroup [group-id unit-map]
  jt.c/HasUnits
  (units [_] unit-map)
  (unit* [_ k] (k unit-map))
  Object
  (toString [_] group-id))

;;;;;;;;; UNIT

(def iso-units
  (vals
    #?(:clj (jt.u/get-static-fields-of-type IsoFields TemporalUnit)
       :cljs (jt.u/get-fields-matching IsoFields #".*YEARS")))) ;todo

(def chrono-units
  (vals
    #?(:clj (jt.u/get-static-fields-of-type ChronoUnit TemporalUnit)
       :cljs (jt.u/get-fields-matching ChronoUnit #".*"))))

(def predefined-units
  (concat iso-units chrono-units))

(def unit-groups
  {:predefined (->map predefined-units)
   :iso (->map iso-units)
   :chrono (->map chrono-units)})

(def ^:dynamic *units* (UnitGroup. :predefined (:predefined unit-groups)))

(extend-type TemporalUnit
  jt.c/KnowsTimeBetween
  (time-between [u t1 t2]
    (.between u ^Temporal t1, ^Temporal t2))

  jt.c/Supporting
  (supports? [u t]
    (.isSupportedBy u ^Temporal t)))

(defn unit?
  "True if this is a `TemporalUnit`."
  [o] (instance? TemporalUnit o))

(defn ^TemporalUnit get-unit [o]
  (cond (unit? o) o

        (keyword? o)
        (jt.c/unit* *units* o)))

(defn ^TemporalUnit get-unit-checked [o]
  (if-let [u (get-unit o)]
    u
    (throw (#?(:clj NullPointerException.
               :cljs js/Error.) (str "No temporal unit found for " o "!")))))

(defn unit-key [o]
  (cond (keyword? o)
        o

        (unit? o)
        (property->key o)))

;;;;;;;;; FIELD

(def iso-fields
  (vals
    #?(:clj (jt.u/get-static-fields-of-type IsoFields TemporalField)
       :cljs (jt.u/get-fields-matching IsoFields #".*"))))

(def julian-fields
  (vals
    #?(:clj (jt.u/get-static-fields-of-type JulianFields TemporalField)
       :cljs {})))

(def chrono-fields
  (vals
    #?(:clj (jt.u/get-static-fields-of-type ChronoField TemporalField)
       :cljs (jt.u/get-fields-matching ChronoField #".*"))))

(def predefined-fields
  (concat iso-fields chrono-fields julian-fields))

;; There is another implementation of fields - WeekFields, which is dynamic

(def field-groups
  {:predefined (->map predefined-fields)
   :iso (->map iso-fields)
   :julian (->map julian-fields)
   :chrono (->map chrono-fields)})

(def ^:dynamic *fields* (FieldGroup. :predefined (:predefined field-groups)))

(extend-type TemporalField
  jt.c/Supporting
  (supports? [f t]
    (.isSupportedBy f ^TemporalAccessor t)))

(extend-type TemporalField
  jt.c/ReadableRangeProperty
  (range [^TemporalField k]
     (.range k))
  (min-value [p]
    ((jt.c/readable-range-property-fns :min-value) p))
  (largest-min-value [p]
    ((jt.c/readable-range-property-fns :largest-min-value) p))
  (smallest-max-value [p]
    ((jt.c/readable-range-property-fns :smallest-max-value) p))
  (max-value [p]
    ((jt.c/readable-range-property-fns :max-value) p)))

(defn field?
  "True if this is a `TemporalField`."
  [o] (instance? TemporalField o))

(defn ^TemporalField get-field [o]
  (cond (field? o)
        o

        (keyword? o)
        (jt.c/field* *fields* o)))

(defn field-key [o]
  (cond (keyword? o)
        o

        (field? o)
        (property->key o)))

(defn ^TemporalUnit unit
  "Returns a `TemporalUnit` for the given key `k` or extracts the field from
  the given temporal `entity`.

  You can see predefined units via `java-time.repl/show-units`.

  If you want to make your own custom TemporalUnits resolvable, you need to rebind the
  `java-time.properties/*units*` to a custom `java-time.properties.UnitGroup`."
  ([k] (get-unit k))
  ([entity k] (jt.c/unit* entity k)))

(defn ^TemporalField field
  "Returns a `TemporalField` for the given key `k` or extracts the field from
  the given temporal `entity`.

  You can see predefined fields via `java-time.repl/show-fields`.

  If you want to make your own custom TemporalFields resolvable, you need to rebind the
  `java-time.properties/*fields*` to a custom `java-time.properties.FieldGroup`."
  ([k] (get-field k))
  ([entity k] (jt.c/field* entity k)))

(extend-type clojure.lang.Keyword
  jt.c/KnowsTimeBetween
  (time-between [k t1 t2]
    (jt.c/time-between (or (get-field k) (get-unit k)) t1 t2))

  jt.c/Supporting
  (supports? [k t]
    (jt.c/supports? (or (get-field k) (get-unit k)) t)))

(extend-type clojure.lang.Keyword
  jt.c/ReadableRangeProperty
  (range [k]
     (jt.c/range (get-field k)))
  (min-value [p]
    ((jt.c/readable-range-property-fns :min-value) p))
  (largest-min-value [p]
    ((jt.c/readable-range-property-fns :largest-min-value) p))
  (smallest-max-value [p]
    ((jt.c/readable-range-property-fns :smallest-max-value) p))
  (max-value [p]
    ((jt.c/readable-range-property-fns :max-value) p)))
