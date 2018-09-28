(ns roland.typesetter3
  (:require [clojure.string :refer [split]]))

;;; * Probabilistic linebreaking

;;; ** Records

(defrecord Box [content
                lenght
                height])

(defrecord Path [current-nlw
                 current-acceptability
                 just-broke?])

(defrecord Branch [trunk acc b-leaf u-leaf])

;;; ** Tokenize text

(defn boxify [text]
  (map #(->Box % (count %) 1) (split text #"\s+")))


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

(defn unbreak-acceptability [i box path tw o- o+]
  (if (< tw (+ (:lenght box) (:current-nlw path)))
    (acc-distribution (:lenght box) (:current-nlw path) o- o+)
    (:current-acceptability path)))

;;; ** Main loop

(defn explore-branch [i box branch tw o- o+]
  (let [bb-leaf (acc-distribution (:lenght box)
                                  (:current-nlw (:b-leaf branch))
                                  o- o+)
        bn-leaf (unbreak-acceptability i box (:b-leaf branch) tw o- o+)
        b-branch (map->Branch {:trunk (conj (:trunk branch) i)
                               :acc (* (:acc branch) (:current-acceptability (:b-leaf branch)))
                               :b-leaf (assoc (:b-leaf branch)
                                              :current-nlw (:lenght box)
                                              :current-acceptability bb-leaf
                                              :just-broke? true)
                               :u-leaf (assoc (:u-leaf branch)
                                              :current-nlw (:lenght box)
                                              :current-acceptability (* (bn-leaf) (:current-acceptability (:b-leaf branch)))
                                              :just-broke? false)})
        nb-leaf (acc-distribution (+ 1 (:lenght box))
                                  (:current-nlw (:u-leaf branch)))
        nn-leaf (unbreak-acceptability i box (:u-leaf branch) tw o- o+)
        n-branch (map->Branch {:trunk (:trunk (:u-leaf branch))
                               :acc (* (:acc branch) (:current-acceptability (:u-leaf branch)))
                               :b-leaf (->Path (+ 1 (:lenght box))
                                               nb-leaf
                                               :just-broke? true)
                               :u-leaf (->Path (+ 1 (:lenght box))
                                               nn-leaf
                                               :just-broke? false)})]))

(defn viterbi-step [box branch tw o- o+]
  (let [new-branchs (explore-branch box branch tw o- o+)]
    ))

(defn viterbi [boxes tw o- o+]
  (let [init-box (first boxes)
        init-boxes (next boxes)
        init-branch (->Branch
                     [0] 1
                     (->Path (:lenght init-box) 1 true)
                     (->Path (:lenght init-box)
                             (acc-distribution (:lenght init-box)
                                               (- tw) o- o+) true))]
    (loop [i 1
           boxes boxes
           branchs init-branch]
      (if (<= 1 (count boxes))
        branchs
        (recur (inc i)
               (next boxes)
               (viterbi-step i (first boxes) branchs tw o- o+))))))

;;; {:broken (->Path ) , :not-broken}

(defn break-paragraph [text tw o- o+]
  (let [boxes (vec (boxify text))]
    (loop [breakups (:breakups (viterbi boxes tw o- o+))
           par []]
      (if (>= 1 (count breakups))
              par
              (recur (next breakups)
                     (conj par (subvec boxes (first breakups) (second breakups))))))))
