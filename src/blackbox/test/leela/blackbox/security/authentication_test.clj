(ns leela.blackbox.security.authentication-test
  (:use     [clojure.test]
            [clj-time.core   :only [minutes
                                    ago
                                    now
                                    from-now]])
  (:require [pandect.core   :as auth]
            [leela.blackbox.security.authentication :as authentication]))

(deftest test-security-authentication-check-signature

    (testing "given a message and a key, returns the correct signature"
      (is (= (auth/md5-hmac "message" "secret") (authentication/signature "message" "secret"))))
  
    (testing "given a different message and key, should fail"
      (is (not= (auth/md5-hmac "egassem" "terces") (authentication/signature "message" "secret"))))
  )

(deftest test-security-authentication-time-validation

    (testing "timestamp inside window."
      (is (= true (authentication/valid-time? (-> 9 minutes from-now) now 10))))

    (testing "timestamp outside window."
      (is (= false (authentication/valid-time? (-> 11 minutes from-now) now 10))))
  )
