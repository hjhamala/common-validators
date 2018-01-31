(ns common-validators.core-test
  (:require [clojure.test :refer :all]
            [common-validators.core :refer :all]))

(deftest valid-business-identity-code-test
  (testing "Nil business identity code is not valid"
    (is (not (valid-business-identity-code? nil))))
  (testing "Identity code which is less than 5 character is not valid"
    (is (not (valid-business-identity-code? "15728-0"))))
  (testing "Identity code which have incorrect checksum character is not valid"
    (is (not (valid-business-identity-code? "1572860-1"))))
  (testing "1572860-0 is valid identity code"
    (is (valid-business-identity-code? "1572860-0")))
  (testing "737546-2 is valid identity code"
    (is (valid-business-identity-code? "737546-2"))))

(deftest validate-finnish-mobile-phone-number-test
  (testing "Phone number must be at least 7 characters long"
    (is (valid-finnish-mobile-phone-number? "0505748767")))
  (testing "6 characters long phone number is not valid"
    (is (not (valid-finnish-mobile-phone-number? "050574"))))
  (testing "+359 is not a valid prefix"
    (is (not (valid-finnish-mobile-phone-number? "+359505748766"))))
  (testing "+358 is a valid prefix"
    (is (valid-finnish-mobile-phone-number? "+358505748766")))
  (testing "050 is valid prefix"
    (is (valid-finnish-mobile-phone-number? "0505748766")))
  (testing "+359505748 is not a valid phone number"
    (is (not (valid-finnish-mobile-phone-number? "+359505748"))))
  (testing "055 is not valid prefix"
    (is (not (valid-finnish-mobile-phone-number? "0555748767")))))