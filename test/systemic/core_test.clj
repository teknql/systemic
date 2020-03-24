(ns systemic.core-test
  (:require [systemic.core :as sut :refer [defsys with-system with-isolated-registry]]
            [systemic.internal :as internal]
            [clojure.test :refer [deftest testing is use-fixtures]]))

(def start-called (atom false))
(def stop-called (atom false))
(def dependent-started (atom false))
(def transitive-dependent-started (atom false))

(use-fixtures :each (fn [f]
                      (reset! start-called false)
                      (reset! stop-called false)
                      (reset! dependent-started false)
                      (reset! transitive-dependent-started false)
                      (f)
                      (sut/stop!)))

(defsys ^:symbol-meta *config*
  "Some documentation"
  {:attr-meta true}
  :start
  (reset! start-called true)
  {:foo 5}
  :stop
  (reset! stop-called true)
  *config* ;; testing we can be self-refrerential
  :foo)

(defsys *dependent*
  :start
  (reset! dependent-started true)
  (:foo *config*))

(defsys *transitive-dependent*
  :start
  *dependent*
  (reset! transitive-dependent-started true))

(def registry-symbol
  "Hardcoded definition of our expected registry symbol. Using something more dynamic
  becomes sensitive to the namespace that the code is evalated in.

  This will break if we rename this file but at least it's defined in just one place."
  'systemic.core-test/*config*)

(deftest find-dependencies-test
  (testing "correctly identifies dependencies in a body"
    (is (= #{registry-symbol}
           (internal/find-dependencies (-> *ns* str symbol) `(get *config* :foo) @sut/*registry*))))

  (testing "handling boddies with java classes"
    (is (= #{} (internal/find-dependencies (-> *ns* str symbol) `(try 5
                                                                      (catch Exception e
                                                                        nil))
                                           @sut/*registry*)))))

(deftest defsys-test
  (testing "registers it in the registry"
    (is (get @sut/*registry* registry-symbol)))

  (testing "start and stop fns"
    (let [start-fn             (-> @sut/*registry* (get registry-symbol) :closure)
          {:keys [value stop]} (start-fn)]
      (is (= {:foo 5} value))
      (is (= :foo (stop)))))

  (testing "metadata inheritance"
    (let [meta (meta #'*config*)]
      (testing "attr-map"
        (is (:attr-meta meta)))
      (testing "symbol metadata"
        (is (:symbol-meta meta)))
      (testing "documentation string" (is (= "Some documentation"
                                             (:doc meta))))))

  (testing "sets the initial variable binding to not running"
    (let [data (ex-data *config*)]
      (is (= ::sut/not-running (:type data)))
      (is (= registry-symbol (:system data)))))

  (testing "short-hand definition"
    (with-isolated-registry
      (defsys *short*
        5)
      (sut/start! `*short*)
      (is (= 5 *short*)))

    (with-isolated-registry
      (defsys *short-string*
        "some string")
      (sut/start! `*short-string*)
      (is (= "some string" *short-string*))))

  (testing "dependencies"
    (testing "sets registry info"
      (is (= #{registry-symbol}
             (-> @sut/*registry* (get `*dependent*) :dependencies))))

    (testing "with explicitly set additional dependencies"
      (with-isolated-registry
        (defsys *actual-dep*
          nil)

        (defsys *explicit-dep*
          nil)

        (defsys *extra-deps*
          :deps [*explicit-dep*]
          :start
          (*actual-dep*)
          5)

        (is (= `#{*actual-dep* *explicit-dep*}
               (-> @sut/*registry* (get `*extra-deps*) :dependencies))))))

  (testing "redefinition semantics"
    (with-isolated-registry
      (let [original-start-called (atom false)
            original-stop-called  (atom false)
            new-start-called      (atom false)
            new-stop-called       (atom false)
            dependent-restarted   (atom false)]

        (defsys *redefined*
          :start (reset! original-start-called true)
          :stop (reset! original-stop-called true))

        (defsys *depends-on-redefined*
          :deps #{*redefined*}
          (reset! dependent-restarted true))
        (sut/start! `*redefined* `*depends-on-redefined*)

        (is @original-start-called)
        (reset! original-start-called false)
        (reset! dependent-restarted false)

        (defsys *redefined*
          :start (reset! new-start-called true)
          :stop (reset! new-stop-called true))

        (is @original-stop-called)
        (is @new-start-called)
        (is @dependent-restarted)
        (is (not @original-start-called))
        (reset! original-stop-called false)

        (is (sut/running? `*redefined*))
        (sut/stop! `*redefined*)
        (is (not @original-stop-called))
        (is @new-stop-called)))))

(deftest start-restart-stop-test
  (testing "start"
    (testing "returns the started systems"
      (is (= `(*config* *dependent* *transitive-dependent*)
             (sut/start!))))
    (testing "rebinds the variables at the root"
      (is (= {:foo 5} *config*)))

    (testing "calls the start function"
      (is @start-called))

    (testing "does not call start for already running systems"
      (reset! start-called false)
      (is (nil? (sut/start!)))
      (is (not @start-called)))

    (testing "resolves as running?"
      (is (sut/running? `*config*)))

    (testing "starts only specified systems when specified"
      (sut/stop!)
      (sut/start! `*config*)
      (is (not (sut/running? `*dependent*)))
      (is (not (sut/running? `*transitive-dependent*)))
      (is (sut/running? `*config*))

      (sut/start! `*transitive-dependent*)
      (is (sut/running? `*dependent*))
      (is (sut/running? `*transitive-dependent*))))

  (testing "restart"
    (reset! stop-called false)
    (reset! start-called false)
    (testing "returns the restarted systems"
      (is (= `(*config* *dependent* *transitive-dependent*)
             (sut/restart!))))
    (testing "calls stop"
      (is @stop-called))
    (testing "calls start"
      (is @start-called))

    (testing "restarts dependents"
      (is (= `(*config* *dependent* *transitive-dependent*) (sut/restart! `*config*)))
      (is (every? sut/running? `(*config* *dependent* *transitive-dependent*)))))

  (testing "stop"
    (reset! stop-called false)
    (testing "returns the stopped systems"
      (is (= `(*transitive-dependent* *dependent* *config*)
             (sut/stop!))))
    (testing "unbinds the variables at the root"
      (is (= ::sut/not-running (:type (ex-data *config*)))))

    (testing "resolves as not running"
      (is (not (sut/running? `*config*))))

    (testing "calls the stop function"
      (is @stop-called))

    (testing "does not call stop for already stopped systems"
      (reset! stop-called false)
      (is (nil? (sut/stop!)))
      (is (not @stop-called)))

    (testing "stops only specified systems when explicitly specified"
      (reset! stop-called false)
      (sut/start!)
      (sut/stop! `*dependent*)
      (is (not @stop-called)))))

(deftest error-handling-test
  (with-isolated-registry
    (defsys *error-on-start*
      (throw (ex-info "Boom" {})))

    (defsys *error-on-stop*
      :stop
      (throw (ex-info "Boom" {})))

    (testing "start errors"
      (try
        (sut/start! `*error-on-start*)
        (catch Exception e
          (let [data (ex-data e)]
            (is (= :system-start (:type data)))
            (is (= `*error-on-start* (:system data)))))))

    (testing "stop errors"
      (sut/start! `*error-on-stop*)
      (try
        (sut/stop! `*error-on-stop*)
        (catch Exception e
          (let [data (ex-data e)]
            (is (= :system-stop (:type data)))
            (is (= `*error-on-stop* (:system data)))))))))

(deftest with-system-test
  (testing "overwrites bindings"
    (with-isolated-registry
      (let [original-start-called (atom false)
            original-val-called   (atom false)
            mock-val-called       (atom false)]
        (defsys *will-be-mocked*
          :start
          (reset! original-start-called true)
          #(reset! original-val-called true)
          )
        (defsys *depends-on-mocked*
          :start
          (*will-be-mocked*))
        (with-system [*will-be-mocked* #(reset! mock-val-called true)]
          (sut/start! `*depends-on-mocked*)
          (is (not @original-start-called))
          (is (not @original-val-called))
          (is @mock-val-called))
        (sut/stop!))))

  (testing "prevents un-needed dependencies from being started"
    (with-isolated-registry
      (let [a-started  (atom false)
            b-started  (atom false)
            c-started  (atom false)
            d-started  (atom false)
            e-started  (atom false)
            reset-all! #(doseq [a [a-started b-started c-started d-started e-started]]
                          (reset! a false))]

        (defsys *a*
          :start
          (reset! a-started true))

        (defsys *b*
          :start
          *a*
          (reset! b-started true))

        (defsys *c*
          :start
          *b*
          (reset! c-started true))

        (defsys *d*
          :start
          *a*
          (reset! d-started true))

        (defsys *e*
          :start
          *c*
          *d*
          (reset! e-started true))

        (testing "direct dependencies"
          (with-system [*b* true]
            (sut/start! `*c*)
            (is @c-started)
            (is (not @b-started))
            (is (not @a-started))
            (reset-all!)))

        (testing "transitive dependencies"
          (with-system [*c* true]
            (sut/start! `*e*)
            (is @e-started)
            (is @d-started)
            (is @a-started)
            (is (not @b-started))
            (is (not @c-started))
            (reset-all!)))))))

(deftest defsys-closure-test
  (with-isolated-registry
    (let [counter (atom 0)]
      (defsys *custom-closure*
        :closure
        {:value @counter
         :stop  #(swap! counter inc)})

      (sut/start! `*custom-closure*)
      (is (= 0 @counter))
      (is (= 0 *custom-closure*))
      (sut/stop! `*custom-closure*)
      (is (= 1 @counter))
      (sut/start! `*custom-closure*)
      (is (= 1 *custom-closure*)))))
