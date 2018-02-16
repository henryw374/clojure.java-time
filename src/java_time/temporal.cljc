(ns java-time.temporal
  (:require [clojure.string :as string]
            [java-time.core :as jt.c :refer (value)]
            [java-time.util :as jt.u]
            [java-time.properties :as jt.p]
            [java-time.format :as jt.f]
            [java-time.clock :as jt.clock]
    [java-time.temporal-macros :as jt.tm :include-macros true]
            #?(:clj
            [java-time.defconversion :refer (deffactory conversion!)]))
  #?(:clj
     (:import [java.time.temporal Temporal TemporalAccessor ValueRange
                                  TemporalField TemporalUnit TemporalAmount ChronoField IsoFields]
              [java.time.format DateTimeFormatter]
              [java.time.chrono Chronology]
              [java.time DateTimeException Clock
                         Period Duration MonthDay DayOfWeek Month Year
                         ZoneOffset Instant])))


#?(:cljs
   (do
     (def Temporal (.. js/JSJoda -Temporal))
     (def TemporalAccessor (.. js/JSJoda -TemporalAccessor))
     (def ValueRange (.. js/JSJoda -ValueRange))
     (def TemporalField (.. js/JSJoda -TemporalField))
     (def TemporalUnit (.. js/JSJoda -TemporalUnit))
     (def TemporalAmount (.. js/JSJoda -TemporalAmount))
     (def ChronoField (.. js/JSJoda -ChronoField))
     (def IsoFields (.. js/JSJoda -IsoFields))
     (def DateTimeFormatter (.. js/JSJoda -DateTimeFormatter))
     ;(def Chronology (.. js/JSJoda -Chronology))
     (def DateTimeException (.. js/JSJoda -DateTimeException))
     (def Clock (.. js/JSJoda -Clock))
     (def Period (.. js/JSJoda -Period))
     (def Duration (.. js/JSJoda -Duration))
     (def MonthDay (.. js/JSJoda -MonthDay))
     (def DayOfWeek (.. js/JSJoda -DayOfWeek))
     (def Month (.. js/JSJoda -Month))
     (def Year (.. js/JSJoda -Year))
     (def ZoneOffset (.. js/JSJoda -ZoneOffset))
     (def Instant (.. js/JSJoda -Instant))
     
     (def ResolverStyle (.. js/JSJoda -ResolverStyle))))


(jt.tm/value-property DayOfWeek (jt.u/static-prop ChronoField 'DAY_OF_WEEK))
(jt.tm/value-property Month (jt.u/static-prop ChronoField 'MONTH_OF_YEAR))
(jt.tm/value-property Year (jt.u/static-prop ChronoField 'YEAR_OF_ERA))
(jt.tm/value-property ZoneOffset (jt.u/static-prop ChronoField 'OFFSET_SECONDS)
  :with-value-fn-sym ofTotalSeconds
  :get-value-fn-sym getTotalSeconds)

#?(:clj
   (jt.u/when-threeten-extra
     (import [org.threeten.extra AmPm DayOfMonth DayOfYear Quarter YearQuarter])
     (jt.tm/value-property DayOfMonth ChronoField/DAY_OF_MONTH)
     (jt.tm/value-property DayOfYear ChronoField/DAY_OF_YEAR)))

;;;;; FIELD PROPERTY

(defn- get-field-property-range [^TemporalAccessor ta, ^TemporalField field]
  (.range ta field))

(defn- get-long-property-value [^TemporalAccessor ta, ^TemporalField field]
  (.getLong ta field))

(defn- quarter->month [q]
  (Math/min 12 (Math/max 1 (long (inc (* 3 (dec q)))))))

(defrecord MonthDayFieldProperty [^MonthDay o, ^TemporalField field]
  jt.c/WritableProperty
  (with-value [_ v]
    (condp = field
      ChronoField/DAY_OF_MONTH (.withDayOfMonth o v)
      ChronoField/MONTH_OF_YEAR (.withMonth o v)
      IsoFields/QUARTER_OF_YEAR (.withMonth o (quarter->month v)))))

(alter-meta! #'->MonthDayFieldProperty assoc :private true)
(alter-meta! #'map->MonthDayFieldProperty assoc :private true)

(defrecord DayOfWeekFieldProperty [^DayOfWeek o, ^TemporalField field]
  jt.c/WritableProperty
  (with-value [_ v]
    (condp = field
      ChronoField/DAY_OF_WEEK (DayOfWeek/of v))))

(alter-meta! #'->DayOfWeekFieldProperty assoc :private true)
(alter-meta! #'map->DayOfWeekFieldProperty assoc :private true)

(defrecord MonthFieldProperty [^Month o, ^TemporalField field]
  jt.c/WritableProperty
  (with-value [_ v]
    (condp = field
      ChronoField/MONTH_OF_YEAR (Month/of v)
      IsoFields/QUARTER_OF_YEAR (Month/of (quarter->month v)))))

(alter-meta! #'->MonthFieldProperty assoc :private true)
(alter-meta! #'map->MonthFieldProperty assoc :private true)

(defrecord ZoneOffsetFieldProperty [^ZoneOffset o, ^TemporalField field]
  jt.c/WritableProperty
  (with-value [_ v]
    (condp = field
      ChronoField/OFFSET_SECONDS (ZoneOffset/ofTotalSeconds v))))

(alter-meta! #'->ZoneOffsetFieldProperty assoc :private true)
(alter-meta! #'map->ZoneOffsetFieldProperty assoc :private true)

(defrecord TemporalFieldProperty [^Temporal o, ^TemporalField field]
  jt.c/WritableProperty
  (with-value [_ v] (.with o field v)))

(alter-meta! #'->TemporalFieldProperty assoc :private true)
(alter-meta! #'map->TemporalFieldProperty assoc :private true)

(field-property DayOfWeekFieldProperty true)
(field-property MonthFieldProperty true)
(field-property MonthDayFieldProperty true)
(field-property TemporalFieldProperty true)
(field-property ZoneOffsetFieldProperty true)

;;;;; FACTORY

(defprotocol PropertyFactory
  (mk-property [factory entity prop-key prop-obj]))

(def default-field-property-factory
  (reify PropertyFactory
    (mk-property [_ e _ field]
      (condp instance? e
        Temporal (TemporalFieldProperty. e field)
        Month (MonthFieldProperty. e field)
        DayOfWeek (DayOfWeekFieldProperty. e field)
        MonthDay (MonthDayFieldProperty. e field)
        ZoneOffset (ZoneOffsetFieldProperty. e field)))))

(def ^:dynamic *field-property-factory* default-field-property-factory)

;;;;; ACCESSOR

(extend-type TemporalAccessor
  jt.c/Supporting
  (supports? [o k]
    (.isSupported o (jt.p/field k)))

  jt.c/HasFields
  (field* [o k]
    (when-let [f (jt.p/field k)]
      (when (jt.c/supports? o f)
        f)))

  (fields [o]
    (let [fs (jt.c/fields jt.p/*fields*)]
      (loop [[k f] (first fs)
             r (rest fs)
             res (transient {})]
        (if f
          (recur (first r) (rest r)
            (if (jt.c/supports? o f)
              (assoc! res k f)
              res))
          (persistent! res)))))

  jt.c/HasProperties
  (properties [o]
    (jt.u/map-kv
      (fn [k p] [k (mk-property *field-property-factory* o k p)])
      (jt.c/fields o)))

  (property [o k]
    (let [f-k (jt.p/field-key k)]
      (if-let [f (jt.c/field* o k)]
        (mk-property *field-property-factory* o f-k f)
        (throw (DateTimeException. (str "Property " k " doesn't exist in [" o "]!")))))))

;;;;;;;;; RANGE

(defn ^ValueRange value-range
  "Creates a `ValueRange` given the `min` and `max` amounts or a map of
  `:min-smallest`, `:max-smallest`, `:min-largest` and `:max-largest`."
  ([min max]
   (value-range {:min-smallest min, :max-smallest max
                 :min-largest min, :max-largest max}))
  ([{:keys [min-smallest min-largest max-smallest max-largest]}]
   (ValueRange/of min-smallest min-largest max-smallest max-largest)))

;;;;;;;;; AMOUNT

(defrecord TemporalAmountUnitProperty [^TemporalAmount ta, ^TemporalUnit unit]
  jt.c/ReadableProperty
  (value [_]
    (.get ta unit)))

(alter-meta! #'->TemporalAmountUnitProperty assoc :private true)
(alter-meta! #'map->TemporalAmountUnitProperty assoc :private true)

(defrecord PeriodUnitProperty [^Period p, unit-key]
  jt.c/ReadableProperty
  (value [_]
    (case unit-key
      :years (.getYears p)
      :months (.getMonths p)
      :days (.getDays p)))

  jt.c/WritableProperty
  (with-value [_ v]
    (case unit-key
      :years (.withYears p v)
      :months (.withMonths p v)
      :days (.withDays p v))))

(alter-meta! #'->PeriodUnitProperty assoc :private true)
(alter-meta! #'map->PeriodUnitProperty assoc :private true)

(defrecord DurationUnitProperty [^Duration d, unit-key]
  jt.c/ReadableProperty
  (value [_]
    (case unit-key
      :seconds (.getSeconds d)
      :nanos (.getNano d)))

  jt.c/WritableProperty
  (with-value [_ v]
    (case unit-key
      :seconds (.withSeconds d v)
      :nanos (.withNanos d v))))

(alter-meta! #'->DurationUnitProperty assoc :private true)
(alter-meta! #'map->DurationUnitProperty assoc :private true)

(def default-unit-property-factory
  (reify PropertyFactory
    (mk-property [_ e unit-key unit]
      (condp instance? e
        Period (PeriodUnitProperty. e unit-key)
        Duration (DurationUnitProperty. e unit-key)
        TemporalAmount (TemporalAmountUnitProperty. e unit)))))

(def ^:dynamic *unit-property-factory* default-unit-property-factory)

(extend-type TemporalAmount
  jt.c/Supporting
  (supports? [o k]
    (not (nil? (jt.c/unit* o (jt.p/get-unit k)))))

  jt.c/HasUnits
  (unit* [o k]
    (when-let [u (jt.p/get-unit k)]
      (first (filter #(= u %) (.getUnits o)))))

  (units [o]
    (let [[u & us] (.getUnits o)]
      (loop [u u, us us, res (transient {})]
        (if u
          (recur (first us) (rest us)
            (assoc! res (jt.p/unit-key u) u))
          (persistent! res)))))

  jt.c/HasProperties
  (properties [o]
    (jt.u/map-kv
      (fn [k p] [k (mk-property *unit-property-factory* o k p)])
      (jt.c/units o)))

  (property [o k]
    (let [u-k (jt.p/unit-key k)]
      (if-let [u (jt.c/unit* o k)]
        (mk-property *unit-property-factory* o u-k u)
        (throw (DateTimeException. (str "Property " k " doesn't exist in [" o "]!")))))))

;;;;;;;;; TEMPORAL

(defn ^Temporal t-plus [^Temporal acc, ^TemporalAmount o]
  (.plus acc o))

(defn ^Temporal t-minus [^Temporal acc, ^TemporalAmount o]
  (.minus acc o))

(extend-type Temporal
  jt.c/Plusable
  (seq-plus [o os]
    (reduce t-plus o os))

  jt.c/Minusable
  (seq-minus [o os]
    (reduce t-minus o os))

  jt.c/KnowsTimeBetween
  (time-between [o e u]
    (.until o ^Temporal e (jt.p/get-unit u)))

  jt.c/KnowsIfLeap
  (leap? [o]
    (when-let [year (-> (jt.c/property o :year)
                        (jt.c/value))]
      (if (satisfies? jt.c/HasChronology o)
        (.isLeapYear (jt.c/chronology o) year)
        (Year/isLeap year))))

  jt.c/As
  (as* [o k]
    (jt.c/value (jt.c/property o k))))

;;;;;; Instant

(conversion! Clock Instant
  (fn [^Clock c]
    (Instant/now c)))

(conversion! java.util.Date Instant
  (fn [^java.util.Date dt]
    (.toInstant dt)))

(conversion! java.util.Calendar Instant
  (fn [^java.util.Calendar c]
    (.toInstant c)))

(conversion! CharSequence Instant
  (fn [^CharSequence s]
    (Instant/parse s))
  2)

(conversion! Number Instant
  (fn [^Number m]
    (Instant/ofEpochMilli (long m))))

(conversion! [DateTimeFormatter CharSequence] Instant
  #(Instant/from (jt.f/parse %1 %2)))

(deffactory instant
  "Creates an `Instant`. The following arguments are supported:

    * no arguments - current instant
    * one argument
      + clock
      + java.util.Date/Calendar
      + another temporal entity
      + string representation
      + millis from epoch
    * two arguments
      + formatter (format) and a string"
  :returns Instant
  :implicit-arities [1 2]
  ([] (jt.clock/make #(Instant/now %))))

(extend-type Instant
  jt.c/Truncatable
  (truncate-to [o u]
    (.truncatedTo o (jt.p/get-unit-checked u)))

  jt.c/Ordered
  (single-after? [d o]
    (.isAfter d o))
  (single-before? [d o]
    (.isBefore d o)))
