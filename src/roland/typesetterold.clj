;;; * Namespace
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

;;; * Linebreaking 1 (Tex like)

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
              (<= (:width p) maxw))]

            (let [[w & ws :as wws] (reverse words)]
              (apply (partial min-key :cost) (reduce pstepr (pstart w) ws)))))

;;; * Linebreaking 3 (Functional linear)
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


;;; * Probabilistic linebreaking

(defrecord BoxP [^java.lang.String content
                 ^java.lang.Integer lenght])

(defrecord LineP [^clojure.lang.PersistentVector boxes 
                  ^java.lang.Integer nlw
                  ^clojure.lang.Numbers acceptability])

(defrecord ParagraphP [^clojure.lang.PersistentVector lines
                       ^roland.typesetter.LineP current-line
                       ^clojure.lang.Numbers current-nlw
                       ^clojure.lang.Numbers acceptability])

;;; ** Test

(defrecord ViterbiPath [^clojure.lang.PersistentVector breakups
                        ^clojure.lang.Numbers nlw
                        ^clojure.lang.Numbers acceptability])

(defrecord BoxV [^java.lang.String content
                 ^clojure.lang.Numbers lenght
                 ^clojure.lang.Numbers height])

(defn normal [x o]
  (/ (Math/exp (/ (Math/pow x 2) (Math/pow o 2)))
     (* 2 Math/PI o)))

(defn normal2 [x o]
  (/ (Math/exp (- (/ (Math/pow x 2) (* 2 (Math/pow o 2)))))
     (* o (Math/sqrt (* 2 Math/PI)))))

(defn boxify [text]
  (map #(->BoxV % (count %) 1) (split text #"\s+")))

(defn acc-distribution
  ([cwi x o- o+] (if (>= (+ cwi x) 0)
                   (/ (normal (+ cwi x) o+) 
                      (acc-distribution x o- o+))
                   (/ (normal (+ cwi x) o-)
                      (acc-distribution x o- o+))))
  ([x o- o+] (if (>= x 0)
               (normal x o+)
               (normal x o-))))

(defn path-acceptability [box path tw o- o+]
  (let [nlwi-1 (:nlw path)
        cwi (if (= nlwi-1 0)
               (:lenght box)
               (+ 1 (:lenght box)))]
    (println nlwi-1 (:acceptability path) (acc-distribution cwi (- nlwi-1 tw) o- o+))
    (* (:acceptability path)
       (acc-distribution cwi (- nlwi-1 tw) o- o+))))

(defn best-breakup [i path1 path2 box tw o- o+]
  (let [p1-acc (path-acceptability box path1 tw o- o+)
        p2-acc (path-acceptability box path2 tw o- o+)]
    (println path1 "\n" path2 "\n" 
       (assoc path1 :nlw (+ (:nlw path1) (:lenght box) 1) :acceptability p1-acc :breakups (conj (:breakups path1) i)) "\n"
       (assoc path2 :nlw (+ (:nlw path2) (:lenght box) 1) :acceptability p2-acc :breakups (conj (:breakups path2) i)))
(if (< p1-acc p2-acc)
      (assoc path1 :nlw (+ (:nlw path1) (:lenght box) 1) :acceptability p1-acc :breakups (conj (:breakups path1) i))
      (assoc path2 :nlw (+ (:nlw path2) (:lenght box) 1) :acceptability p2-acc :breakups (conj (:breakups path2) i)))))

(defn non-break-acceptability [path box tw o- o+]
  (let [cwi (if (= 0 (:nlw path))
              (:lenght box)
            (+ 1 (:lenght box)))] 
    (if (< tw (+ (:nlw path) cwi))
      (path-acceptability box path tw o- o+)
      (:acceptability path))))

(defn best-non-break [path1 path2 box tw o- o+]
  (let [p1-acc (non-break-acceptability path1 box tw o- o+) 
        p2-acc (non-break-acceptability path2 box tw o- o+)
        cwi (if (= 0 (:nlw path2))
              (:lenght box)
              (+ 1 (:lenght box)))] 
    (println (assoc path1 :acceptability p1-acc :nlw (+ cwi (:nlw path1))))
    (println (assoc path2 :acceptability p2-acc :nlw (+ cwi (:nlw path2))))
    (if (< p1-acc p2-acc) 
      (assoc path1 :acceptability p1-acc :nlw (+ cwi (:nlw path1)))
      (assoc path2 :acceptability p2-acc :nlw (+ cwi (:nlw path2))))))

(defn break-paragraph [text breaks]
  (loop [brks breaks
         par []]
    (if (>= 1 (count brks))
      par
      (recur (next brks) (conj par (subvec text (first brks) (second brks)))))))

(defn viterbi [text tw o- o+]
  (loop [i 1
         boxes (boxify text)
         path1 (->ViterbiPath [0] 0 1)
         path2 (->ViterbiPath [0] 0 1)]
    ;; (println i)
    (if (= 1 (count boxes))
      (break-paragraph (vec (boxify text)) (:breakups (best-breakup i path1 path2 (first boxes) tw o- o+)))
      (recur (inc i) 
             (next boxes) 
             (best-breakup i path1 path2 (first boxes) tw o- o+)
             (best-non-break path1 path2 (first boxes) tw o- o+)))))

#_(defn breakup-acceptability [box par tw o+ o-]
  ;;only if a breakpoint candidat
  (let [nlwi-1 (:nlw (:current-line par))
        cwi(if (= nlwi-1 0)
              (:lenght box)
              (+ 1 (:lenght box))) 
        prev-a (:acceptability par)]
    (br-acceptability-distribution cwi (- nlwi-1 tw) o+ o-)))

;;; ** Natural line width

#_(defn nlwi [cwi nlwi-1 in]
  (if nlwi-1 
    (if (nlwi-1 :breakup)
      cwi
      (+ (nlwi-1 :width) cwi))
    in))

;;; ** Acceptability probability

(defn pies [x o+ o-]
            (if (> x 0)
              (normal x o+)
              (normal x o-)))

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

#_(defn prob-linebreak-step [par box tw o+ o-]
  (let [acc (breakup-acceptability box par tw o+ o-)]
    (println (:content box) (:nlw (:current-line par))  acc)
    (if (<= acc (:acceptability (:current-line par)))
      (add-box! par box acc)
      (new-line! par box acc))))

#_(defn prob-linebreak [text text-width o+ o-]
  (let [line (->LineP [] 0 1)
        paragraph (->ParagraphP [] line 0 1)
        breakup-step #(prob-linebreak-step %1 %2 text-width o+ o-)]
    (reduce breakup-step paragraph (boxify text))))


;;; ** Functions

;;; *** To maximize

(defn c1 [o+ o-]
(/ (Math/pow o+ 2) (Math/pow o- 2)))

(defn c2 [o+ o-]
(* (Math/pow o+ 2) (Math/log (/ o+ o-))))

(defn c3 [o+]
(* (Math/pow o+ 2) (Math/log (* 2 Math/PI o+))))

 
