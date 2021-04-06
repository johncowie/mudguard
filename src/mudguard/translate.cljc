(ns mudguard.translate
  (:require [mudguard.core :as core]))

(defn coerce-to-vec [x]
  (if (coll? x)
    x
    [x]))

(defn matches-translation? [error-path translation-id]
  (if (empty? error-path)
    false
    (or (= error-path translation-id)
        (matches-translation? (rest error-path) translation-id))))

(defn map-over-keys [f m]
  (->> (for [[k v] m]
         [(f k) v])
       (into {})))

(defn map-over-vals [f m]
  (->> (for [[k v] m]
         [k (f v)])
       (into {})))

(defn find-translation [error-path translations]
  (->> translations
       (map-over-keys coerce-to-vec)
       (sort-by (comp count first))
       reverse                                              ;; TODO optimise, do this upfront
       (filter (comp (partial matches-translation? error-path) first))
       (map second)
       first))

(defn- error-paths [errors]
  (->> errors
       ::core/errors
       (map ::core/id)))

(defn translate-errors [translations errors]
  (let [paths (error-paths errors)]
    (->> (for [path paths]
           [(drop-last path) (find-translation path translations)])
         (group-by first)
         (map-over-vals (partial map second)))))

(defn- add-error-msg [m [[path-f & path-r] messages]]
  (cond (and (empty? m) (nil? path-f))
        messages
        (nil? m)
        {path-f (add-error-msg {} [path-r messages])}
        (sequential? m)
        {::errors m
         path-f (add-error-msg {} [path-r messages])}
        :else
        (update m path-f add-error-msg [path-r messages])
        ))

(defn restructure-messages [error-messages]
  (->> error-messages
       (reduce add-error-msg {})))
