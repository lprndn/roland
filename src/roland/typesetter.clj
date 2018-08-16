;;;_ Namespace
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

;;;_ * Linebreaking 1 (Tex like)

(defrecord Par1 [^clojure.lang.PersistentList ls ^java.lang.Long width ^java.lang.Long cost])

#_(defn lb1 [words maxw optw]
    (letfn [
            (pstart [word]
              ;;{[l & ls] width cost}
              (list (Par1.  (list (list word)) (:length word) 0)))

            (pstepr [ps w]
              (filter pfit (conj (map #(pglue w %1) ps)
                                 (pnew w (apply (partial min-key :cost) ps)))))

            (pnew [w p]
              (let [ls (:ls p)
                    cost (:cost p)]
                
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
            (let [[w & ws :as wws] (reverse words)]
              (apply (partial min-key :cost) (reduce pstepr (pstart w) ws)))]))

;;;_ * Linebreaking 3 (Functional linear)
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
               totw (+ w tw 1)]
               
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
                       (- tw (p 0) 1)))]

                   
             (Par3. (ptrim (dropnofit (padd (pnew (last ps)) ps))) totw (+ tl 1)))))

       (ptile
         [ws mm n]
         (loop [
                ws ws
                [m & ms :as mms] mm
                n n
                output []]
                
           (if (or (nil? m) (< n 0))
             output
             (let [
                   l (- n m)
                   [ws1 ws2] (split-at l ws)]
                   
               (recur ws2 (drop l mms) (- n l) (conj output ws1))))))]
       
    (let [[w & sw :as wsw] (rseq (vec (tokenize ws))) 
          zs (reductions pstepr (pstart (:width w)) (map :width sw))] 
      (map #(map :content %1) (ptile wsw (reverse (map #(-> %1 :ps last last) zs)) (:tl (last zs)))))))


;;;_ * Probabilistic linebreaking

(defrecord BoxP [^java.lang.String content
                 ^java.lang.Integer lenght])

(defrecord LineP [^clojure.lang.PersistentVector boxes 
                  ^java.lang.Integer nlw
                  ^clojure.lang.Numbers acceptability])

(defrecord ParagraphP [^clojure.lang.PersistentVector lines
                       ^roland.typesetter.LineP current-line
                       ^clojure.lang.Numbers current-nlw
                       ^clojure.lang.Numbers acceptability])

;;;_   * Test

(defn normal [x o]
  (/ (Math/exp (/ (Math/pow x 2) (Math/pow o 2)))
     (* 2 Math/PI o)))


(defn boxify [text]
  (map #(->BoxP % (count %)) (split text #"\s+")))

;;;_   * Natural line width

#_(defn nlwi [cwi nlwi-1 in]
  (if nlwi-1 
    (if (nlwi-1 :breakup)
      cwi
      (+ (nlwi-1 :width) cwi))
    in))

;;;_   * Acceptability probability

(defn br-acceptability-distribution
  ([cwi x o+ o-] (if (>= (+ cwi x) 0)
                   (do (println "+")(/ (normal (+ cwi x) o+)
                       (br-acceptability-distribution x o+ o-)))
                   (do (println "-") (/ (normal (+ cwi x) o-)
                          (br-acceptability-distribution x o+ o-)))))
  ([x o+ o-] (if (>= x 0)
               (do (println "2+") (normal x o+))
               (do (println "2-") (normal x o-)))))

(defn breakup-acceptability [box par tw o+ o-]
  ;;only if a breakpoint candidat
  (let [nlwi-1 (:nlw (:current-line par))
        cwi (if (= nlwi-1 0)
              (:lenght box)
              (+ 1 (:lenght box)))
        prev-a (:acceptability par)]
    (br-acceptability-distribution cwi (- nlwi-1 tw) o+ o-)))

(defn new-line! [par box acc]
  (let [nline (->LineP [box] (:lenght box) acc)
        nlines (conj (:lines par) (:current-line par))
        new-par (assoc par
                        :lines nlines
                        :current-line nline)]
    new-par))

(defn add-box! [par box acc]
  (let [updated-boxes (conj (:boxes (:current-line par)) box)
        updated-nlw (+ (if (= (:nlw (:current-line par)) 0) 0 1) (:lenght box) (:nlw (:current-line par)))
        updated-line (assoc (:current-line par) 
                             :boxes updated-boxes
                             :acceptability acc
                             :nlw updated-nlw)
        new-par (assoc par :current-line updated-line)]
    new-par))

(defn prob-linebreak-step [par box tw o+ o-]
  (let [acc (breakup-acceptability box par tw o+ o-)]
    (println (:content box) (:nlw (:current-line par))  acc)
    (if (<= acc (:acceptability (:current-line par)))
      (add-box! par box acc)
      (new-line! par box acc))))

(defn prob-linebreak [text text-width o+ o-]
  (let [line (->LineP [] 0 1)
        paragraph (->ParagraphP [] line 0 1)
        breakup-step #(prob-linebreak-step %1 %2 text-width o+ o-)]
    (reduce breakup-step paragraph (boxify text))))


;;;_  * Functions

;;;_   * To maximize

(defn c1 [o+ o-]
(/ (Math/pow o+ 2) (Math/pow o- 2)))

(defn c2 [o+ o-]
(* (Math/pow o+ 2) (Math/log (/ o+ o-))))

(defn c3 [o+]
(* (Math/pow o+ 2) (Math/log (* 2 Math/PI o+))))

 
