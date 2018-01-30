(ns java-time.util
  (:require [clojure.string :as string]
            #?@(:clj
            [[java-time.util-macros]
             [java-time.potemkin.namespaces :as potemkin]]))
  #?(:clj
     (:import [java.lang.reflect Field])))

#?(:clj 
   (potemkin/import-vars 
     [java-time.util-macros static-prop static-call]))

#?(:clj
   (defn get-static-fields-of-type [^Class klass, ^Class of-type]
     (->> (seq (.getFields klass))
          (map (fn [^Field f]
                 (when (.isAssignableFrom of-type (.getType f))
                   [(.getName f) (.get f nil)])))
          (keep identity)
          (into {})))
   :cljs 
   (defn get-fields-matching [obj regex]
     (->>
       (js-keys obj)
       (filter #(re-matches regex %))
       (map #(vector % (goog.object/get obj %)))
       (into {}))))

(defn dashize [camelcase]
  (let [words (re-seq #"([^A-Z]+|[A-Z]+[^A-Z]*)" camelcase)]
    (string/join "-" (map (comp string/lower-case first) words))))


#?(:clj
   (defmacro if-threeten-extra [then-body else-body]
     (if (try (Class/forName "org.threeten.extra.Temporals")
              (catch Throwable e))
       `(do ~then-body)
       `(do ~else-body)))
   :cljs (defn if-threeten-extra [] false))

#?(:clj
   (defmacro when-threeten-extra [& body]
     (if (try (Class/forName "org.threeten.extra.Temporals")
              (catch Throwable e))
       `(do ~@body)))
   :cljs (defn when-threeten-extra []))

#?(:clj
   (defmacro when-joda-time-loaded
     "Execute the `body` when Joda-Time classes are found on the classpath.
   
     Take care - when AOT-compiling code using this macro, the Joda-Time classes
     must be on the classpath at compile time!"
     [& body]
     (if (try (Class/forName "org.joda.time.DateTime")
              (catch Throwable e))
       `(do ~@body)))
   :cljs (defn when-joda-time-loaded []))

;; From Medley, C Weavejester
(defn editable? [coll]
  #?(:clj  (instance? clojure.lang.IEditableCollection coll)
     :cljs (satisfies? cljs.core.IEditableCollection coll)))

(defn reduce-map [f coll]
  (if (editable? coll)
    (persistent! (reduce-kv (f assoc!) (transient (empty coll)) coll))
    (reduce-kv (f assoc) (empty coll) coll)))

(defn map-vals
  "Maps a function over the values of an associative collection."
  [f coll]
  (reduce-map (fn [xf] (fn [m k v] (xf m k (f v)))) coll))

(defn map-kv
  "Maps a function over the key/value pairs of an associate collection. Expects
  a function that takes two arguments, the key and value, and returns the new
  key and value as a collection of two elements."
  [f coll]
  (reduce-map (fn [xf] (fn [m k v] (let [[k v] (f k v)] (xf m k v)))) coll))
