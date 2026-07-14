(ns systemic.reload-test
  (:require [systemic.core :as sut]
            [clojure.test :refer [deftest testing is]]))

(def clj-reload-available?
  (try
    (requiring-resolve 'clj-reload.core/keep-methods)
    true
    (catch Throwable _
      false)))

(deftest keep-methods-registration-test
  (testing "registers a keep-method for the defsys tag when clj-reload is present"
    (if clj-reload-available?
      (let [keep-methods @(requiring-resolve 'clj-reload.core/keep-methods)
            defs-method   @(requiring-resolve 'clj-reload.keep/keep-methods-defs)]
        (is @#'sut/-clj-reload-installed?)
        (doseq [tag '[defsys systemic.core/defsys]]
          (testing (str "tag " tag)
            (is (= defs-method (keep-methods tag))))))
      (testing "no-op and returns false when clj-reload is absent"
        (is (false? @#'sut/-clj-reload-installed?))))))
