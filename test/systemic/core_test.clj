(ns systemic.core-test
  (:require [systemic.core :as sut :refer [defsys]]
            [clojure.test :refer [deftest testing is use-fixtures]]))

(def start-called (atom false))
(def stop-called (atom false))

(use-fixtures :each (fn [f]
                      (reset! start-called false)
                      (reset! stop-called false)
                      (f)))

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
  (:foo *config*))

(def registry-symbol
  "Hardcoded definition of our expected registry symbol. Using something more dynamic
  becomes sensitive to the namespace that the code is evalated in.

  This will break if we rename this file but at least it's defined in just one place."
  'systemic.core-test/*config*)

(deftest find-dependencies-test
  (testing "correctly identifies dependencies in a body"
    (is (= #{registry-symbol}
           (#'sut/find-dependencies
             `(get *config* :foo)
             @sut/*registry*)))))

(deftest defsys-test
  (testing "registers it in the registry"
    (is (get @sut/*registry* registry-symbol)))
  (testing "creates a start function"
    (let [start-fn (-> @sut/*registry* (get registry-symbol) :start)]
      (is (= {:foo 5} (start-fn)))))
  (testing "creates a stop function"
    (let [stop-fn (-> @sut/*registry* (get registry-symbol) :stop)]
      (is (= :foo (stop-fn)))))

  (testing "metadata inheritance"
    (let [meta (meta #'*config*)]
      (testing "attr-map"
        (is (:attr-meta meta)))
      (testing "symbol metadata"
        (is (:symbol-meta meta)))
      (testing "documentation string"
        (is (= "Some documentation"
               (:doc meta))))))

  (testing "sets the initial variable binding to not running"
    (let [data (ex-data *config*)]
      (is (= ::sut/not-running (:type data)))
      (is (= registry-symbol (:system data)))))

  (testing "dependencies"
    (testing "sets registry info"
      (is (= #{registry-symbol}
             (-> @sut/*registry* (get `*dependent*) :dependencies)))))

  (testing "redefinition semantics"
    (let [original-start-called (atom false)
          original-stop-called  (atom false)
          new-start-called      (atom false)
          new-stop-called       (atom false)]

      (defsys *redefined*
        :start (reset! original-start-called true)
        :stop (reset! original-stop-called true))

      (sut/start! [`*redefined*])

      (is @original-start-called)
      (reset! original-start-called false)

      (defsys *redefined*
        :start (reset! new-start-called true)
        :stop (reset! new-stop-called true))

      (is @original-stop-called)
      (is @new-start-called)
      (is (not @original-start-called))
      (reset! original-stop-called false)

      (sut/stop! [`*redefined*])
      (is (not @original-stop-called))
      (is @new-stop-called))

    (sut/forget! `*redefined*)))


(deftest start-order
  (binding [sut/*registry* (atom {`*config*   {:dependencies #{}}
                                  `*db*       {:dependencies #{`*config*}}
                                  `*indexer*  {:dependencies #{`*db*}}
                                  `*web*      {:dependencies #{`*db*}}
                                  `*isolated* {:dependencies #{}}})]
    (testing "returns a sorted list of all systems if no systems are specified"
      (is (= `(*config* *db* *web* *indexer* *isolated*)
             (sut/start-order))))

    (testing "returns a subset of systems if systems are specified"
      (is (= `(*config*) (sut/start-order `(*config*))))
      (is (= `(*config* *db* *web*) (sut/start-order `(*web*))))
      (is (= `(*config* *db* *web*) (sut/start-order `(*db* *web*)))))))


(deftest start-restart-stop-test
  (testing "start"
    (testing "returns the started systems"
      (is (= `(*config* *dependent*)
             (sut/start!))))
    (testing "rebinds the variables at the root"
      (is (= {:foo 5} *config*)))
    (testing "associates the variable in the *system* variable"
      (is (= {:foo 5} (get sut/*system* `*config*))))

    (testing "calls the start function"
      (is @start-called))

    (testing "does not call start for already running systems"
      (reset! start-called false)
      (is (nil? (sut/start!)))
      (is (not @start-called)))

    (testing "resolves as running?"
      (is (sut/running? `*config*))))

  (testing "restart"
    (reset! stop-called false)
    (reset! start-called false)
    (testing "returns the restarted systems"
      (is (= `(*config* *dependent*)
             (sut/restart!))))

    (testing "calls stop"
      (is @stop-called))
    (testing "calls start"
      (is @start-called)))

  (testing "stop"
    (reset! stop-called false)
    (testing "returns the stopped systems"
      (is (= `(*dependent* *config*)
             (sut/stop!))))
    (testing "unbinds the variables at the root"
      (is (= ::sut/not-running (:type (ex-data *config*)))))

    (testing "removes the variable from the *system* variable"
      (is (not (contains? sut/*system* `*config*))))

    (testing "resolves as not running"
      (is (not (sut/running? `*config*))))

    (testing "calls the stop function"
      (is @stop-called))

    (testing "does not call stop for already stopped systems"
      (reset! stop-called false)
      (is (nil? (sut/stop!)))
      (is (not @stop-called)))))
