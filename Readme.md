# Systemic
[![Clojars Project](https://img.shields.io/clojars/v/teknql/systemic.svg)](https://clojars.org/teknql/systemic)
[![cljdoc badge](https://cljdoc.org/badge/teknql/systemic)](https://cljdoc.org/d/tekqnl/systemic/CURRENT)

> “Industrialization is the systemic exploitation of wasting assets. In all too
> many cases, the thing we call progress is merely an acceleration in the rate
> of that exploitation.”
> — Aldous Huxley

## Motivation and Comparison to Other Libraries

Clojure has many fantastic solutions for application state management -
[component](https://github.com/stuartsierra/component),
[mount](https://github.com/tolitius/mount), and
[integrant](https://github.com/weavejester/integrant) are all fantastic
libraries which bring unique and valuable features to the table. They serve as
great inspiration

`systemic` is similar to `mount`, in that it strives to prioritize the
experience in the repl and makes use of clojure's own resolution
capabilities to implicitly define dependencies between components. Additionally
it uses dynamic scope to allow for multiple isolated systems in a single repl
allowing for testing of systems in the same repl as development.

## Examples / Features

### Implicit and Explicit Dependency Resolution

```clojure
(ns example.dep-resolution
  (:require [systemic.core :as systemic :refer [defsys]]))

(defsys *port*
  :start (read-string (System/getenv "APPLICATION_PORT")))

(defsys *server*
  :start (start-web-server *port*)
  :stop (shutdown-server *server*))

(defsys *monitor*
  :extra-deps [*server*]
  :start
  (start-monitor!)
  :stop
  (kill-monitor!))

(systemic/start!)
;; => ('example.dep-resolution/*port* 'example.dep-resolution/*server* 'example.dep-resoltuion/*monitor*)
```

### Isolated Environments

Using the `systemic/with-system` creates an isolated environment where none of
the existing systems are running.

```clojure
(defsys *api-key*
  (->> (io/resource "secrets.edn")
       (slurp)
       (read-string)
       :api-key))

(defn decorate-req
  [req]
  (-> req
      (update :url #(str "https://api.example.com" %))
      (assoc :as :json)
      (assoc-in [:headers "API-KEY"] *api-key*)
      (assoc-in [:headers "USer-Agent"] "Shiny Co. API Client")))

(defstate *send-req*
  :extra-deps [*api-key*]
  :start
  (comp http/request decorate-req))

(defn fetch-users
  []
  (:body (*send-req* {:method :get
                      :url    "/users"})))

;; Example test
(deftest fetch-users-test
  (let [req (atom nil)]
    (with-system [*send-req* #(do (reset! req %)
                                  {:body [{:name "Bob"}
                                          {:name "Steve"}]})]
      ;; Note that because we have our own definition for `*send-req*` above,
      ;; dependencies (ie. `*api-key*`) of the original `*send-req*` will not be started.
      (systemic/start! `*send-req*)
      (is (= (fetch-users)
             [{:name "Bob"}
              {:name "Steve"}])))))
```

## Caveats and Warnings

Systemic makes heavy use of dynamic scope and there are a few sharp edges to be
aware of when working with it. Dynamic bindings are thread-local, which means
that when you create a new thread you need to use `bound-fn` to ensure that the
local bindings in the current thread make their way to the new thread.
Fortunately, most of clojure's built in concurrency primitives and libraries
handle this for you behind the scenes.

## Roadmap

- [ ] Restart Strategies (see Mount for inspiration)
