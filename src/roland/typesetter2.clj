(ns roland.typesetter2
  (:require [clojure.string :refer [split]]))

;;; * Probabilistic linebreaking

;;; ** Records

(defrecord Box [content
                lenght
                height])

(defrecord Path [breakups
                 current-nlw
                 current-acceptability
                 just-broke?])

;;; ** Tokenize text

(defn boxify [text]
  (map #(->Box % (count %) 1) (split text #"\s+")))

;; ** Path comparison

(defn normal [x o]
  (/ (Math/exp (/ (Math/pow x 2) (Math/pow o 2)))
     (* 2 Math/PI o)))

(defn normal2 [x o]
  (/ (Math/exp (- (/ (Math/pow x 2) (* 2 (Math/pow o 2)))))
     (* o (Math/sqrt (* 2 Math/PI)))))

(defn acc-distribution
  ([cwi x o- o+] (if (>= (+ cwi x) 0)
                   (/ (normal (+ cwi x) o+)
                      (acc-distribution x o- o+))
                   (/ (normal (+ cwi x) o-)
                      (acc-distribution x o- o+))))
  ([x o- o+] (if (>= x 0)
               (normal x o+)
               (normal x o-))))

(defn explore-path [i box path tw o- o+]
  (println i (:current-nlw path))
  (let [cwi (if (:just-broke? path) (:lenght box) (+ 1 (:lenght box)))
        broken-path (assoc path
                           :breakups (conj (:breakups path) i)
                           :current-nlw 0 #_(if (:just-broke? path) (:lenght box)  (+ (:lenght box) cwi))
                           :current-acceptability (* (:current-acceptability path)
                                                     (acc-distribution (+ 0 (:lenght box)) (- (:current-nlw path) tw)  o- o+))
                           :just-broke? true)
        unbroken-path (assoc path
                             :current-nlw (if (:just-broke? path) (:lenght box) (+ (:current-nlw path) cwi))
                             :current-acceptability (if (< tw (+ 1 (:lenght box) (:current-nlw path)))
                                                      (* (:current-acceptability path)
                                                         (acc-distribution (+ 1 (:lenght box))
                                                                           (- (:current-nlw path) tw) o- o+))
                                                      (:current-acceptability path))
                             :just-broke? false)]
    [broken-path unbroken-path]))

(defn viterbi-step [i box paths tw o- o+]
  (let [possible-paths (map #(explore-path i box % tw o- o+) paths)
        break-path (apply max-key :current-acceptability  (map first possible-paths))
        unbroken-path (apply max-key :current-acceptability  (map second possible-paths))]
     (println possible-paths ())
    [break-path unbroken-path]))

;;; ** Main loop


(defn viterbi [boxes tw o- o+]
    (loop [i 0
           boxes boxes
           paths [(->Path [0] 0 1 true)]]
      (if (empty? boxes)
        (do  (first paths))
        (recur (inc i) (next boxes) (viterbi-step i (first boxes) paths tw o- o+)))))

(defn break-paragraph [text tw o- o+]
  (let [boxes (vec (boxify text))]
    (loop [breakups (:breakups (viterbi boxes tw o- o+))
           par []]
      (if (>= 1 (count breakups))
              par
              (recur (next breakups)
                     (conj par (subvec boxes (first breakups) (second breakups))))))))
