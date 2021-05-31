(ns mudguard.translate
  (:require [mudguard.core :as core]))

(defn coerce-to-vec [x]
  (if (coll? x)
    x
    [x]))

(defn matches-translation? [error-path translation-id]
  (if (empty? error-path)
    false
    (or (= (remove int? error-path) translation-id)
        (matches-translation? (rest error-path) translation-id))))

(defn map-over-keys [f m]
  (->> (for [[k v] m]
         [(f k) v])
       (into {})))

(defn map-over-vals [f m]
  (->> (for [[k v] m]
         [k (f v)])
       (into {})))

(defn create-translation [error translation-or-f]
  (if (fn? translation-or-f)
    (translation-or-f error)
    translation-or-f))

(defn find-translation [error-path error translations]
  (->> translations
       (map-over-keys coerce-to-vec)
       (sort-by (comp count first))
       reverse                                              ;; TODO optimise, do this upfront
       (filter (comp (partial matches-translation? error-path) first))
       (map second)
       first
       (create-translation error)))

(defn- error-paths [errors]
  (->> errors
       ::core/errors
       (map (juxt ::core/id identity))))

(defn translate-errors [translations errors]
  (let [paths-and-errors (error-paths errors)]
    (->> (for [[path error] paths-and-errors]
           [(drop-last path) (find-translation path error translations)])
         (group-by first)
         (map-over-vals (partial map second))
         (map-over-keys vec))))

(defn- add-error-msg [m [[path-f & path-r] messages]]
  (cond (and (empty? m) (nil? path-f))
        messages
        (nil? m)
        {path-f (add-error-msg {} [path-r messages])}
        (sequential? m)
        {::errors m
         path-f   (add-error-msg {} [path-r messages])}
        :else
        (update m path-f add-error-msg [path-r messages])
        ))

(defn restructure-messages [error-messages]
  (->> error-messages
       (reduce add-error-msg {})))

(defn untranslatable-errors [translations errors]
  (let [error-list (::core/errors errors)
        missing-translations (->> error-list
                                  (remove (fn [e] (find-translation (::core/id e) e translations))))]
    (when-not (empty? missing-translations)
      {::core/errors missing-translations})))

(defn missing-translations [translations validator]
  (some->> validator
           (core/possible-errors)
           (untranslatable-errors translations)))
