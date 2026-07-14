# Systemic
[![Build Status](https://img.shields.io/github/workflow/status/teknql/systemic/CI.svg)](https://github.com/teknql/systemic/actions)
[![Clojars Project](https://img.shields.io/clojars/v/teknql/systemic.svg)](https://clojars.org/teknql/systemic)
[![cljdoc badge](https://cljdoc.org/badge/teknql/systemic)](https://cljdoc.org/d/teknql/systemic/CURRENT)

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
great inspiration for systemic.

`systemic` is similar to `mount`, in that it strives to prioritize the
experience in the REPL and makes use of clojure's own resolution
capabilities to implicitly define dependencies between components. Additionally
it uses dynamic scope to allow for multiple isolated systems in a single REPL
allowing for testing of systems in the same REPL as development.

## Examples / Features

### Implicit and Explicit Dependency Resolution

The `defsys` macro will automatically infer dependent systems by analyzing the
body of the form. Additional dependencies can be specified by explicitly adding
them with the `:deps` option of the macro.

```clojure
(ns example.dep-resolution
  (:require [systemic.core :as systemic :refer [defsys]]))

(defsys *port*
  :start (read-string (System/getenv "APPLICATION_PORT")))

(defsys *server*
  :start (start-web-server *port*)
  :stop (shutdown-server *server*))

(defsys *monitor*
  :deps [*server*]
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
  "The API key for the application"
  ;; A system with no :start or :stop is presumed to only have a :start
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

(defsys *send-req*
  :deps [*api-key*]
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

### Redefinition

If a system is running, and redefined, systemic will stop it (using the old
definition) and then start it using the new definition. Additionally, all
dependent systems will be restarted. Ultimately systemic will allow this
behavior to be configurable (see Roadmap below).

```clojure
(defsys *a*
  :start
  (println "Old A start")
  :stop
  (println "Old A stop"))

(defsys *b*
  :deps [*a*]
  :start
  (println "B start")
  :stop
  (println "B stop"))

(systemic/start!)
;; => ('example/*a* 'example/*b*)
;; Prints:
;; Old A start
;; B start
```

Now imagine re-evaluating the buffer / expression with a new definition of
`*a*`:

```clojure
(defsys *a*
  :start (println "New A start")
  :stop (println "New A Stop"))

;; Evaluating, since `*a*` is already running, causes the following to be
;; printed:
;; B stop
;; Old A stop
;; New A start
;; B start
```


### Error Handling

If an exception is thrown during the starting or stopping of systems, systemic
will keep the application in a partial state and re-throws the exception with
data about the offending system. This makes it easy to be in the same state as
when the exception was thrown, hopefully making it easier to fix. It also avoids
having to re-provision potentailly expensive dependencies, thereby speeding up
the development experience.

```clojure
(defsys *socket*
  :start (start-socket! {:port 8080})
  :stop (close-socket! *socket*))

(defsys *socket-consumer*
  :start
  (throw (ex-info "Boom" {})))

(try
  (systemic/start! `*socket-consumer*)
  (catch Exception e
    (let [{:keys [cause type system]} (ex-data e)]
      (println (case type
                 :system-start "Error during start"
                 :system-stop  "Error during stop"))
      (= 'example/*socket* system) ;; => true
      (println "Original exception"
               cause) ;;
      )))

```

### Custom Closures

Sometimes it can be useful to capture variables inside of a running system. To
provide for this, systemic provides the `:closure` option as an alternative to
the `:start` and `:stop` configuration.

The `:closure` body will be invoked each time the system starts and should return
a map with `:value` and `:stop`. The `:value` key will be used to set the
running system's value, while `:stop` should be a function that will be called
when the system is stopped.

```clojure
(defsys *advanced-system*
  :closure
  (let [my-internal-server (create-server!)]
    {:value (wrap-server my-internal-server)
     :stop  (fn [] (stop-server! my-internal-server))}))
```

## Caveats and Warnings

Systemic makes heavy use of dynamic scope and there are a few sharp edges to be
aware of when working with it. Dynamic bindings are thread-local, which means
that when you create a new thread you need to use `bound-fn` to ensure that the
local bindings in the current thread make their way to the new thread.
Fortunately, most of clojure's built in concurrency primitives and libraries
handle this for you behind the scenes.

### Reloading with clj-reload

[clj-reload](https://github.com/tonsky/clj-reload) is a smarter way to reload
Clojure code during development. When a namespace is reloaded, its systems are,
by default, stopped (using the old definition) and restarted (using the new
one) - exactly like the redefinition semantics above.

Some systems are expensive to recreate and don't need to be restarted just
because your code changed (a database connection or web server, for example).
For these, annotate the system with `^:clj-reload/keep` and systemic will
preserve its running value across reloads instead of restarting it:

```clojure
(ns example.server
  (:require [systemic.core :as systemic :refer [defsys]]))

;; Restarted on every reload of this namespace (the default)
(defsys *worker*
  :start (start-worker!)
  :stop (stop-worker! *worker*))

;; Kept running across reloads
^:clj-reload/keep
(defsys *server*
  :start (start-web-server! *port*)
  :stop (shutdown-server! *server*))
```

Systemic wires this up automatically: if clj-reload is on the classpath when
`systemic.core` loads, it teaches clj-reload how to keep `defsys` forms. When
clj-reload is absent this is a no-op, so it remains an optional, development-only
dependency. The integration relies on `defsys` being referred (or fully
qualified as `systemic.core/defsys`) in the form.

## Editor and Tool Configuration

Below are some useful snippets to make working with systemic even better.

### Clj-Kondo

Place this in `$PROJECT_ROOT/.clj-kondo/config.edn` to get proper linting inside
from clj-kondo.
``` clojure
{:lint-as {systemic.core/defsys clojure.core/def}}
```


### Emacs

```elisp
;; Fix docstring highlighting for `defsys`
(put 'defsys 'clojure-doc-string-elt 2)

;; The below functions allow you to control systemic from Emacs.
;; Personally, I have found binding them to keys to be very convenient.
(defun systemic/restart ()
  "Restarts all systemic systems"
  (interactive)
  (cider-interactive-eval "(systemic.core/restart!)"))

(defun systemic/start ()
  "Starts all systemic systems"
  (interactive)
  (cider-interactive-eval "(systemic.core/start!)"))

(defun systemic/stop ()
  "Stops all systemic systems"
  (interactive)
  (cider-interactive-eval "(systemic.core/stop!)"))
```


## Roadmap

- [ ] Restart Strategies (see Mount for inspiration)
- [ ] Lifecycle hooks (on-start, on-stop, on-restart, on-error)
- [ ] Explore clojurescript
