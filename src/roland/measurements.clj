;; * Namespace
(ns roland.measurements)

;; * Functions
(defrecord Measurement [value unit context])

(def units {
            "pt" (Measurement. 1 "pt" "absolute")
            "mm" (Measurement. 2.8346457 "pt" "absolute")
            "cm" (Measurement. 10 "mm" "absolute")
            "m" (Measurement. 10 "mm" "absolute")
            "in" (Measurement. 72 "pt" "absolute")
            })

(defn toMeasurement
  "Convert a string to a Measurement"
  [^String string]
  (let [
        value (->> string (re-find #"\d+\.?\d*") read-string)
        unit (re-find #"[a-z]+$" string)
        context (if (nil? (units unit))
                  "relative" "absolute")]
    (Measurement. value unit context)))

(defn toPoints
  "Convert a Measurement to pts."
  [measurement]
  (if (= (:context measurement) "absolute")
    (let [to (units (:unit measurement))
          v (* (:value measurement)
               (:value to))
          converted (Measurement. v (:unit to) "absolute")]
      (if (= (:unit to) "pt")
        converted
        (toPoints converted)))))
