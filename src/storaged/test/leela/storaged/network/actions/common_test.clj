(ns leela.storaged.network.actions.common-test
  (:require
   [clojure.test :refer :all]
   [slingshot.slingshot :refer [try+]]
   [leela.storaged.network.actions.common :refer :all]))

(deftest test-when-map-ok
  (is (when-map {:foo keyword?} {:foo :bar} true)))

(deftest test-when-map-type-fails
  (is
   (try+
    (when-map {:foo integer?} {:foo :bar} false)
    (catch [:type :leela.storaged/user-error] _
      true))))

(deftest test-when-map-key-missing
  (is
   (try+
    (when-map {:foo (constantly true)} {:bar :foo} false)
    (catch [:type :leela.storaged/user-error] _
      true))))

(deftest test-when-map-fun-missing
  (is
   (try+
    (when-map {} {:foo :bar} false)
    (catch [:type :leela.storaged/user-error] _
      true))))
