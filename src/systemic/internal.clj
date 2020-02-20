(ns systemic.internal
  "Internal namespace used by systemic macros"
  (:require [clojure.walk :as walk]))

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
