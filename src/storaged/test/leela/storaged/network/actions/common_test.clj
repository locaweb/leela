(ns leela.storaged.network.actions.common-test
  (:require
   [clojure.test :refer :all]
   [slingshot.slingshot :refer [try+]]
   [leela.storaged.network.actions.common :refer :all]))

(deftest test-when-map
  (is (try+
       (when-map {:foobar integer?} {"foobar" 10} false)
       (catch [:type :leela.storaged/user-error] _
         true))))
