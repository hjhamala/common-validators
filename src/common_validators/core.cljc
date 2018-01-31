(ns common-validators.core)

;; Validate y-tunnus

(def y-tunnus-regex #"(^[0-9]{6,7})-([0-9])")

#?(:cljs
   (defn- str->int [s]
          (js/parseInt (str s)))
   :clj
   (defn- str->int [s]
     (Integer/parseInt (str s))))

(defn- multiply-with-y-tunnus-coefficients
  [ints]
  (mapv * ints [7, 9, 10, 5, 8, 4 2]))

(defn- add-zero-if-length-6
  [s]
  (if (= (count s) 6)
    (cons 0 s)
    s))

(defn valid-business-identity-code?
  "Checks that a submitted code is a valid business identity code (y-tunnus). Supports also old six number Y-tunnus without
  zero padding"
  [code]
  (when-not (or (nil? code) (not (re-matches y-tunnus-regex code)))
    (let [[code-str checksum-str] (clojure.string/split code #"-")
          code-to-number (map str->int code-str)
          checksum-to-number (str->int checksum-str)
          checksum-from-code (->> code-to-number
                                  add-zero-if-length-6
                                  multiply-with-y-tunnus-coefficients
                                  (reduce +)
                                  (#(mod % 11)))]
      (condp = checksum-from-code
        0 (= 0 checksum-to-number)
        1 nil
        (= checksum-to-number (- 11 checksum-from-code ))))))

;; Validate a finnish mobile phone number

(def area-codes
  ["040" "0400" "041" "042" "043" "044" "045" "046"
   "050" "0500"
   "010" "020" "029" "030" "071" "073" "075"
   "0600" "0700" "0800" "0457" "0440"])

(def phone-number-regex #"[+0-9][\s0-9]{7,}")

(defn valid-area-code
  [number]
  (some #(clojure.string/starts-with? number %) area-codes))

(defn has-+358-in-front
  [number]
  (= (subs number 0 4) "+358"))

(defn has+
  [number]
  (= \+ (first number)))

(defn valid-finnish-mobile-phone-number?
  "Validates finnish mobile phone number. Number can have country prefix in syntax +358. Otherwise only
  numbers are allover"
  [number]
  (let [first-character (first number)]
    (and
      (not (nil? number))
      (re-matches phone-number-regex number)
      (if (has+ number)
        (and
          (has-+358-in-front number)
          (valid-area-code (str "0" (subs number 4))))
        (valid-area-code number)))))