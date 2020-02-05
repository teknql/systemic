(ns systemic.core
  (:require [clojure.walk :as walk]
            [clojure.set :as set]
            [loom.graph :as graph]
            [loom.alg :as graph.alg]
            [systemic.core :as sut]))

(def ^:dynamic *registry*
  (atom nil))

(def ^:dynamic *isolated*
  nil)

(def ^:dynamic *system*
  {})

(defn- -set!
  "A variant of `-set!` that sets dynamic variables in the root by editing the root variable in the
  case that we aren't in an isolated scope.

  I'm sorry Rich Hickey!"
  [var-symbol value]
  (if-not *isolated*
    (alter-var-root (resolve var-symbol) (constantly value))
    (eval `(set! ~var-symbol ~value))))

(defn- extract-arg
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

(defn- find-dependencies
  "Utility function which looks at the provided `form` and finds any dependent
  systems using the provided registry."
  [form registry]
  (let [result (transient #{})]
    (walk/postwalk
      (fn [item]
        (when-some [qualified-sym (cond
                                    (qualified-symbol? item) item
                                    (symbol? item)           (symbol (str *ns*) (str item)))]
          (when (contains? registry qualified-sym)
            (conj! result qualified-sym))))
      form)
    (persistent! result)))

(defn start-order
  "Returns the order in which the defined systems will be started.

  Optionally takes a sequence of system symbols, in which case only the provided systems and
  their dependent systems will be considered."
  ([] (start-order nil))
  ([system-symbols]
   (let [reg   @*registry*
         graph (->> reg
                    (mapcat (fn [[k {:keys [dependencies]}]]
                              (map vector (repeat k) dependencies)))
                    (apply graph/add-edges (apply graph/digraph (keys reg))))]
     (if-not (seq system-symbols)
       (-> graph graph.alg/topsort reverse)
       (->> system-symbols
            (map #(->> % (graph.alg/topsort graph) reverse))
            flatten
            distinct)))))

(defn stop-order
  "Returns the order in which the defined systems will be stopped.

  Optionally takes a sequence of system symbols, in which case only the provided systems and
  their dependent systems will be considered."
  ([] (stop-order nil))
  ([system-symbols] (reverse (start-order system-symbols))))


(defn running?
  "Returns whether the provided system is currently running"
  [system-symbol]
  (contains? *system* system-symbol))

(defn- -start!
  "Private helper for `start!`. Returns a system map of state"
  [system-symbols]
  (let [reg @*registry*]
    (into {}
          (map (fn [sys]
                 [sys (when-some [start-fn (-> reg (get sys) :start)]
                        (start-fn))]))
          (remove running? (start-order system-symbols)))))

(defn start!
  "Starts all known systems.

  Optionally takes a sequence of subsystems, in which case only the provided systems
  (and their dependencies) will be started."
  ([] (start! nil))
  ([system-symbols]
   (let [sys-map (-start! system-symbols)]
     (-set! `*system* (merge *system* sys-map))
     (doseq [[sys state] sys-map]
       (-set! sys state))
     (keys sys-map))))

(defn- -stop!
  "Private helper for `stop!`. Returns a seq of stopped symbols"
  [system-symbols]
  (let [reg     @*registry*
        to-stop (->> system-symbols
                     (stop-order)
                     (filter sut/running?))]
    (doseq [sys to-stop]
      (when-some [stop-fn (-> reg (get sys) :stop)]
        (stop-fn)))
    to-stop))

(defn stop!
  "Stops all known systems.

  Optionally takes a sequence of subsystems, in which case only the provided systems
  (and their dependencies) will be started."
  ([] (stop! nil))
  ([system-symbols]
   (let [stopped (-stop! system-symbols)
         sys-map (apply dissoc *system* stopped)]
     (-set! `*system* sys-map)
     (doseq [sys stopped]
       (-set! sys (ex-info "System not running" {:type   ::not-running
                                                 :system sys})))
     (seq stopped))))

(defn restart!
  "Restarts all running systems. Returns a sequence of the restarted systems.

  Optionally takes a sequence of subsystems, in which case only the provided systems will
  be restarted."
  ([] (restart! nil))
  ([system-symbols]
   (let [reg        @*registry*
         to-restart (->> (if (seq system-symbols)
                           system-symbols
                           (keys reg))
                         (filter running?))]
     (stop! to-restart)
     (start! to-restart)
     (seq to-restart))))

(defn forget!
  "Removes the provided `system-symbol` from the registry.

  Returns a sequence of the remaining known systems."
  [system-symbol]
  (keys (swap! *registry* dissoc system-symbol)))


(defn register-system!
  "Registers a system in the `registry`. Called by the `defsys` macro. Use that instead!"
  [system-name data]
  (let [running? (running? system-name)]
    (when running?
      (stop! [system-name]))
    (swap! *registry* assoc system-name data)
    (when running?
      (start! [system-name]))))

(defmacro defsys
  "Defines a new systemic component with the provided name."
  {:arglists '((name doc-string? attr-map? [:start start-body] [:stop stop-body]))}
  [name & args]
  (let [[doc-str args]  (extract-arg args string?)
        [attr-map args] (extract-arg args map?)
        start-body      (->> args
                             (drop-while (comp not #{:start}))
                             (take-while (comp not #{:stop}))
                             seq)
        start-fn        (when start-body
                          `(fn [] ~@start-body))
        stop-body       (->> args
                             (drop-while (comp not #{:stop}))
                             (take-while (comp not #{:start}))
                             seq)

        stop-fn (when stop-body
                  `(fn [] ~@stop-body))

        deps (set/union
               (find-dependencies start-body @*registry*)
               (find-dependencies stop-body @*registry*))

        name          (with-meta name (merge {:dynamic       true
                                              :doc           doc-str
                                              ::system       true
                                              ::dependencies deps}
                                             (meta name)
                                             attr-map))
        qualified-sym (symbol (str *ns*) (str name))]

    `(do
       (declare ~name)
       (register-system! '~qualified-sym
                         {:start        ~start-fn
                          :stop         ~stop-fn
                          :dependencies '~deps})
       (def ~name (ex-info "System not running" {:type   ::not-running
                                                 :system '~qualified-sym})))))
