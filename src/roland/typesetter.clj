;; * Namespace
(ns roland.typesetter
  (:require [clojure.string :refer [split]]))


;; * Functions
;; ** Global
;; width_tl 0
;; cost_tl 1
;; length_tl 2
;; [par width len]

(defprotocol TypeToken
  (tokenize [w]))

(defrecord Box [^java.lang.String content ^java.lang.Integer length]
  TypeToken
  (tokenize [b] b))

(defrecord Glue [^java.lang.Long width ^java.lang.Long stretch ^java.lang.Long shrink]
  TypeToken
  (tokenize [g] g))

(defrecord Penalty [^java.lang.Long width ^java.lang.Long cost]
  TypeToken
  (tokenize [p] p))

(extend-protocol TypeToken
  java.lang.String
  (tokenize [s]
    (map #(Box. %1 (count %1)) (split s #"\s+"))))

;; ** Linebreaking 1 (Tex like)
(defrecord Par1 [^clojure.lang.PersistentList ls ^java.lang.Long width ^java.lang.Long cost])

(defn lb1 [words maxw optw]
  (letfn [
          (pstart [word]
            ;;{[l & ls] width cost}
            (list (Par1.  (list (list word)) (:length word) 0)))

          (pstepr [ps w]
            (filter pfit (conj (map #(pglue w %1) ps)
                               (pnew w (apply (partial min-key :cost) ps)))))

          (pnew [w p]
            (let [
                  ls (:ls p)
                  cost (:cost p)
                  ]
              (if (and (= 1 (count ls)) (= 0 cost))
                (Par1. (conj ls (list w)) (:length w) 0)
                (Par1. (conj ls (list w)) (:length w) (pcost p)))))

          (pglue [w p]
            (let [[l & ls] (:ls p)]
              (Par1. (conj ls (conj l w)) (+ (:width p) 1 (count w)) (:cost p))))

          (pcost [p]
            (+ (plinc p) (:cost p)))

          (plinc [p]
            (Math/pow (- optw (:width p)) 2))

          (pfit [p]
            (<= (:width p) maxw))
          ](let [[w & ws :as wws] (reverse words)]
            (apply (partial min-key :cost) (reduce pstepr (pstart w) ws)))))

;; ** Linebreaking 3 (Functional linear)
(defrecord Par3 [^clojure.lang.PersistentVector ps ^java.lang.Long tw ^java.lang.Integer tl])

(defn lb3
  [ws maxw optw]
  (letfn
      [
       (pstart [width]
         ;;{[width cost length] width lenght}
         (Par3.  [[0 0 0]] width 1))

       (pstepr
         [{:keys [ps tw tl]} w]
         (let [
               totw (+ w tw 1)
               ]
           (letfn [
                   (pnew [p]
                     ;; if single 
                     (if (= 0 (p 2))
                       [tw 0 tl]
                       [tw (+ (p 1) (Math/pow (- optw (poldwdth p)) 2)) tl]))

                   (padd
                     [p [q r & _ :as ps]]
                     (cond
                       (or (empty? ps) (= 1 (count ps))) (into [p] ps)
                       (<= (pbf p q) (pbf q r)) (padd p (next ps))
                       :else (into [p] ps)))

                   (dropnofit [ps]
                     (cond
                       (empty? ps) ps
                       (> (pwdth (last ps)) maxw) (dropnofit (butlast ps))
                       :else ps))

                   (ptrim [ps]
                     (cond
                       (or (empty? ps) (= 1 (count ps))) ps
                       (<= (pcost (last (butlast ps))) (pcost (last ps))) (ptrim (butlast ps))
                       :else ps))

                   (pbf
                     [p q]
                     (let
                         [wph (pwdth p)
                          wqh (pwdth q)
                          rqh (+ (- maxw wqh) 1)]
                       (cond
                         (and (= 0 (q 2)) (= 0 (p 1))) (min rqh (- optw wph))
                         (= 0 (q 2)) rqh
                         :else (min (Math/ceil (/ (- (pcost p) (pcost q)) (* (- wqh wph) 2)))
                                    rqh))))

                   (pcost [p]
                     (if (= 0 (p 2))
                       0
                       (+ (p 1) (Math/pow (- optw (pwdth p)) 2))))

                   (pwdth [p]
                     (if (= 0 (p 2))
                       totw
                       (- totw (p 0) 1)))

                   (poldwdth [p]
                     (if (= 0 (p 2))
                       tw
                       (- tw (p 0) 1)))

                   ]
             (Par3. (ptrim (dropnofit (padd (pnew (last ps)) ps))) totw (+ tl 1)))))

       (ptile
         [ws mm n]
         (loop [
                ws ws
                [m & ms :as mms] mm
                n n
                output []
                ]
           (if (or (nil? m) (< n 0))
             output
             (let [
                   l (- n m)
                   [ws1 ws2] (split-at l ws)
                   ]
               (recur ws2 (drop l mms) (- n l) (conj output ws1))))))
       ]
    (let [[w & sw :as wsw] (rseq (vec (tokenize ws))) 
          zs (reductions pstepr (pstart (:width w)) (map :width sw))] 
      (map #(map :content %1) (ptile wsw (reverse (map #(-> %1 :ps last last) zs)) (:tl (last zs)))))))


;; ** Probabilistic linebreaking

;; *** Test

(defn plb [tw cws in po no]
  ())

;; *** Functions

(defn normal [x o]
  (* (/ 1 (* 2 Math/PI o))
     (Math/exp (/ (Math/pow x 2) (Math/pow o 2)))))

(defn distribution [cw x o]
  (/ (normal (+ x cw) o)
     (normal x o)))
