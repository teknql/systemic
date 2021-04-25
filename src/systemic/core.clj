(ns systemic.core
  (:require [clojure.set :as set]
            [systemic.internal :as internal]))

(def ^{:dynamic true :no-doc true} *registry*
  (atom {}))

(def ^{:dynamic true :no-doc true} *isolated*
  false)

(defn- -set!
  "A variant of `-set!` that sets dynamic variables in the root by editing the root variable in the
  case that we aren't in an isolated scope.

  I'm sorry Rich Hickey!"
  [var-symbol value]
  (if-not *isolated*
    (alter-var-root (resolve var-symbol) (constantly value))
    (var-set (resolve var-symbol) value)))


(defn running?
  "Returns whether the provided system is currently running"
  [system-symbol]
  (let [v       (resolve system-symbol)
        var-val (when v (var-get v))]
    (cond
      (not v)            false
      (not (bound? v))   false
      (= ::not-running
         (some-> var-val
                 (ex-data)
                 :type)) false
      :else              true)))

(defn dependencies
  "Returns the dependencies of the provided system symbol"
  [system-symbol]
  (some->> system-symbol
           resolve
           symbol
           (get @*registry*)
           :dependencies))

(defn dependents
  "Returns the dependents of the provided system symbol"
  [system-symbol]
  (->> @*registry*
       (keep
         (fn [[s {:keys [dependencies]}]]
           (when (dependencies system-symbol)
             s)))))

(defn start!
  "Starts all known systems.

  If called with symbols, only the provided systems (and their dependencies) will be started."
  [& system-symbols]
  (let [reg @*registry*]
    (loop [started-systems nil
           system-symbols  (->> (if (seq system-symbols)
                                  (->> system-symbols
                                       (map (comp symbol resolve)))
                                  (keys reg))
                                (remove running?)
                                (doall))]
      (if-some [sys (first system-symbols)]
        (if (running? sys)
          (recur started-systems (rest system-symbols))
          (let [{:keys [closure dependencies
                        stop-atom]} (get reg sys)
                needed-deps         (remove running? dependencies)]
            (if (seq needed-deps)
              (recur started-systems (concat needed-deps system-symbols))
              (do (-set! sys (when closure
                               (try
                                 (let [{:keys [value stop]} (closure)]
                                   (reset! stop-atom stop)
                                   value)
                                 (catch Exception e
                                   (throw
                                     (ex-info "Error starting system" {:type   :system-start
                                                                       :system sys
                                                                       :cause  e}))))))
                  (recur (cons sys started-systems)
                         (rest system-symbols))))))
        (when started-systems
          (reverse started-systems))))))

(defn stop!
  "Stops all known systems.

  If called with symbols, only the provided systems will be stopped."
  [& system-symbols]
  (let [reg     @*registry*
        systems (->> (if (seq system-symbols)
                       system-symbols
                       (keys reg))
                     (map (comp symbol resolve)))]
    (loop [to-stop systems
           stopped nil]
      (if-some [system (first to-stop)]
        (if-not (running? system)
          (recur (rest to-stop) stopped)
          (let [running-dependents (filter running? (dependents system))]
            (if (seq running-dependents)
              (recur (concat running-dependents to-stop) stopped)
              (do (when-some [stop-fn (-> reg (get system) :stop-atom deref)]
                    (try
                      (stop-fn)
                      (catch Exception e
                        (throw (ex-info "Error stopping system" {:type   :system-stop
                                                                 :system system
                                                                 :cause  e})))))
                  (-set! system (internal/not-running system))
                  (recur (rest to-stop) (cons system stopped))))))
        (when stopped
          (reverse stopped))))))

(defn restart!
  "Restarts all running systems. Returns a sequence of the restarted systems.

  If called with symbols, only the specified systems will be restarted."
  [& system-symbols]
  (let [reg        @*registry*
        to-restart (->> (if (seq system-symbols)
                          system-symbols
                          (keys reg))
                        (map (comp symbol resolve))
                        (filter running?)
                        (doall))
        stopped    (apply stop! to-restart)]
    (apply start! stopped)))

(defn forget!
  "Removes the provided `system-symbol` from the registry.

  Returns a sequence of the remaining known systems."
  [system-symbol]
  (keys (swap! *registry* dissoc system-symbol)))

(defn state
  "Returns the existing state for the running system, if it exists"
  [system-symbol]
  (when (running? system-symbol)
    (var-get (resolve system-symbol))))


(defn register-system!
  "Registers a system in the `registry`. Called by the `defsys` macro. Use that instead!"
  {:no-doc true}
  [system-name data]
  (let [to-restart (stop! system-name)]
    (swap! *registry* assoc system-name data)
    (when to-restart
      (apply start! to-restart))
    (resolve system-name)))

(defmacro defsys
  "Defines a new systemic component with the provided name."
  {:arglists '((name doc-string? attr-map?
                     [:deps extras]
                     [:start start-body]
                     [:stop stop-body]
                     [:closure closure-body]))}
  [name-symbol & args]
  (let [[doc-str args]          (if (= 1 (count args))
                                  [nil args]
                                  (internal/extract-arg args string?))
        [attr-map args]         (if (= 1 (count args))
                                  [nil args]
                                  (internal/extract-arg args map?))
        {deps         :deps
         start-body   :start
         stop-body    :stop
         closure-body :closure} (internal/extract-kwargs [[:deps :extra-deps]
                                                          :start
                                                          :stop
                                                          :closure]
                                                         args)
        _                       (assert (not (and closure-body
                                                  (or start-body stop-body)))
                                        "Conflicting configuration found. Cannot use `:closure` with `:stop` and `:start`")
        start-body              (if (or start-body stop-body closure-body)
                                  start-body
                                  (seq args))
        closure-fn              (if closure-body
                                  `(fn [] ~@closure-body)
                                  `(fn [] {:value (do ~@start-body)
                                           :stop  (fn [] ~@stop-body)}))
        ns                      (symbol (str *ns*))
        qualified-sym           (symbol (str *ns*) (name name-symbol))
        name-symbol             (with-meta (symbol (name name-symbol))
                                  (merge {:dynamic true
                                          :doc     doc-str
                                          ::system true}
                                         (meta name-symbol)
                                         attr-map))]
    `(let [reg#  @*registry*
           deps# (set/difference (set/union
                                   (internal/find-dependencies '~ns '~deps reg#)
                                   (internal/find-dependencies '~ns '~closure-body reg#)
                                   (internal/find-dependencies '~ns '~start-body reg#)
                                   (internal/find-dependencies '~ns '~stop-body reg#))
                                 #{'~qualified-sym})]
       (def ~name-symbol
         (or (state '~qualified-sym)
             (internal/not-running '~qualified-sym)))
       (register-system! '~qualified-sym
                         {:closure      ~closure-fn
                          :stop-atom    (atom nil)
                          :dependencies deps#}))))

(defmacro with-system
  "Executes `body` using system overrides from `bindings` as running systems."
  [bindings & body]
  (let [bindings (->> bindings
                      (partition 2 2)
                      (mapcat (fn [[s expr]]
                                [`#'~s expr])))]
    `(let [systems#   (keys @*registry*)
           base#      (->> systems#
                           (map (fn [k#]
                                  [(resolve k#) (internal/not-running k#)]))
                           (into {}))
           overrides# (array-map
                        (var *isolated*) true
                        ~@bindings)
           bindings#  (merge base# overrides#)]
       (with-bindings bindings#
         ~@body))))

(defmacro with-isolated-registry
  "Executes `body` with an isolated registry such that `defsys` calls will not be persisted.

  Defined systems will be removed from the calling ns as well."
  [& body]
  `(let [reg#           @*registry*
         known-systems# (set (keys reg#))
         temp-reg#      (atom reg#)]
     (binding [*registry* temp-reg#]
       (try
         ~@body
         (finally
           (let [new-systems# (->> @temp-reg#
                                   (keys)
                                   (remove known-systems#))]
             (doseq [s# new-systems#]
               (ns-unmap  (symbol (namespace s#))
                          (symbol (name s#))))))))))
