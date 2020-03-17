(ns systemic.internal
  "Internal namespace used by systemic macros"
  {:no-doc true}
  (:require [clojure.walk :as walk]
            [clojure.set :as set]))

(defn extract-arg
  "Utility function for extracting arguments from a list.
  Returns a tuple of the value matching the `pred` if it returns logical true and the
  rest of the arg list. Otherwise returns the original arg list.
  Takes an optional `error` which will assert that the `pred` returns a truthy
  value.."
  ([args pred] (extract-arg args pred nil))
  ([args pred error]
   (let [[arg new-args] [(first args) (rest args)]]
     (if error
       (do (assert (pred arg) error)
           [arg new-args])
       (if (pred arg)
         [arg new-args]
         [nil args])))))

(defn extract-kwargs
  "Extract out kwarg sections from the provided `form`.

  Return a map with keys of `keywords` and values corresponding to their
  respective body forms. If the body is empty, the key will be omitted from the map.

  Keywords also supports sets of keywords, in which case they are assumed to be aliases, and
  the first keyword in the set will appear in the map."
  [keywords form]
  (let [all-keys (reduce (fn [all-keys k]
                           (if (coll? k)
                             (set/union all-keys (set k))
                             (conj all-keys k)))
                         #{}
                         keywords)]
    (reduce (fn [acc key-or-set]
              (let [key      (if (coll? key-or-set)
                               (first key-or-set)
                               key-or-set)
                    key-set  (if (coll? key-or-set)
                               (set key-or-set)
                               #{key-or-set})
                    sub-form (->> form
                                  (drop-while (comp not key-set))
                                  (drop 1)
                                  (take-while (comp not all-keys)))]
                (if (seq sub-form)
                  (assoc acc key sub-form)
                  acc)))
            {}
            keywords)))

(defn find-dependencies
  "Utility function which looks at the provided `form` and finds any dependent
  systems using the provided registry."
  [ns form registry]
  (let [result (transient #{})]
    (walk/postwalk
      (fn [item]
        (when-some [qualified-sym (when (symbol? item)
                                    (let [resolved (ns-resolve ns item)]
                                      (cond
                                        (var? resolved)   (symbol resolved)
                                        (nil? resolved)   nil
                                        (class? resolved) nil)))]
          (when (contains? registry qualified-sym)
            (conj! result qualified-sym))))
      form)
    (persistent! result)))


(defn not-running
  "Returns the not-running exception for the provided `system-symbol`"
  [system-symbol]
  (ex-info "System not running" {:type   :systemic.core/not-running
                                 :system system-symbol}))
