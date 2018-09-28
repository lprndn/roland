(ns roland.typesetter
  (:import [org.apache.pdfbox.pdmodel.font PDFont])
  (:require [clojure.string :refer [split]]
            [clojure.pprint :as pp]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defrecord Box [^java.lang.String content
                ^clojure.lang.Numbers length
                ^clojure.lang.Numbers height])

(defrecord Path [breakups
                 ^clojure.lang.Numbers current-nlw
                 ^clojure.lang.Numbers prev-acceptability
                 ^clojure.lang.Numbers current-acceptability])

(defn boxify [text]
  (map #(->Box % (count %) 1) (split text #"\s+")))

(defn text->Boxes [^java.lang.String text
                   ^org.apache.pdfbox.pdmodel.font.PDFont font
                   size]
  (map #(->Box %
               (* size (/ (.getStringWidth font %) 1000)) 1)
       (split text #"\s+")))

;; * Viterbi
(defn line-acceptability [o- o+]
  (fn [x] (let [o (double (if (> 0 x) o- o+))]
            (- (- (double(Math/log (double (* 2 (* Math/PI o))))))
               (/ (double (Math/pow x 2)) (double (Math/pow o 2)))))))

;; TODO Find where to put font settings -> Probably a style map with paragraph and font settings
;; TODO Switch wordspacing calculation directly in typesetter.

(defn viterbi [tw o- o+]
  (fn [^clojure.lang.PersistentVector$2 boxes font]
    (let [line-acc (line-acceptability o- o+)
         space (* 12 (/ (.getWidthFromFont font 32) 1000))

         explore-path (fn [i ^roland.typesetter.Box box ^roland.typesetter.Path path]
                        (let [cwi (:length box)
                              nlw (:current-nlw path)
                              acc (:current-acceptability path)
                              longerline-path (assoc path
                                                     :breakups (assoc (:breakups path) (- (count (:breakups path)) 1) i)
                                                     :current-nlw (+ space cwi nlw)
                                                     :current-acceptability (+ (:prev-acceptability path)
                                                                               (line-acc (- (+ space cwi nlw) tw))))
                              newline-path (assoc path
                                                  :breakups (conj (:breakups path) i)
                                                  :current-nlw cwi
                                                  :current-acceptability (+ acc
                                                                            (line-acc (- cwi tw)))
                                                  :prev-acceptability acc)]
                          [longerline-path newline-path]))

         explore-paths (fn [i ^roland.typesetter.Box box ^clojure.lang.PersistentVector paths]
                         (let [explored (map #(explore-path i box %) paths)
                               lg-lines (apply max-key :current-acceptability (reduce #(conj %1 (first %2)) [] explored))
                               nl-lines (apply max-key :current-acceptability (reduce #(conj %1 (second %2)) [] explored))]
                           [lg-lines nl-lines]))]
     (loop [i 1
            boxes boxes
            paths [(->Path [0 0] 0 1 1)]]
       (if (.hasNext boxes)
         (recur (inc i) boxes (explore-paths i (.next boxes) paths))
         (:breakups (apply max-key :current-acceptability paths)))))))

;; * Tex like

(defrecord Par1 [^clojure.lang.PersistentList ls ^java.lang.Long width ^java.lang.Long cost])


;; TODO Convert to iterator
(defn lb1 [maxw optw]
  (fn [words font]
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
               (Par1. (conj ls (conj l w)) (+ (:width p) (* 12 (/ (.getWidthFromFont font 32) 1000)) (:length w)) (:cost p))))

           (pcost [p]
             (+ (plinc p) (:cost p)))

           (plinc [p]
             (Math/pow (- optw (:width p)) 2))

           (pfit [p]
             (<= (:width p) maxw))]

     (let [[w & ws :as wws] (reverse words)]
       (reduce #(conj % (+ (last %) (count %2))) [0] (:ls (apply (partial min-key :cost) (reduce pstepr (pstart w) ws))))))))


(defn break-paragraph [text font size typesetter]
  (let [bxs (text->Boxes text font size)
        boxes (clojure.lang.RT/iter bxs)]
    (loop [breakups (typesetter boxes font)
           par []]
      (if (>= 1 (count breakups))
          par
          (recur (next breakups)
                 (conj par (subvec (vec bxs) (first breakups) (second breakups))))))))


;; * Archives
#_(defn explore-path [i box path tw o- o+]
  (let [cwi (:lenght box)
        nlw (:current-nlw path)
        acc (:current-acceptability path)
        longerline-path (assoc path
                               :breakups (assoc (:breakups path) (- (count (:breakups path)) 1) i)
                               :current-nlw (+ 1 cwi nlw)
                               :current-acceptability (+ (:prev-acceptability path)
                                                         (line-acceptability (+ 1 cwi nlw) tw o- o+)))
        newline-path (assoc path
                            :breakups (conj (:breakups path) i)
                            :current-nlw cwi
                            :current-acceptability (+ acc
                                                      (line-acceptability cwi tw o- o+))
                            :prev-acceptability acc)]
    [longerline-path newline-path]))

#_(defn explore-paths [i box paths tw o- o+]
  (let [explored (map #(explore-path i box % tw o- o+) paths)
        lg-lines (apply max-key :current-acceptability (reduce #(conj %1 (first %2)) [] explored))
        nl-lines (apply max-key :current-acceptability (reduce #(conj %1 (second %2)) [] explored))]
    [lg-lines nl-lines]))



#_(defn normal [x o]
  (/ (Math/exp (/ (Math/pow x 2) (Math/pow o 2)))
      (* 2 Math/PI o)))

#_(defn normal2 [x o]
  (/ (Math/exp (- (/ (Math/pow x 2) (* 2 (Math/pow o 2)))))
     (* o (Math/sqrt (* 2 Math/PI)))))

#_(defn acc-distribution
  ([cwi x o- o+] (if (>= (+ cwi x) 0)
                   (-
                    (/ (normal (+ cwi x) o+)
                       (acc-distribution x o- o+)))
                   (- 1
                    (/ (normal (+ cwi x) o-)
                       (acc-distribution x o- o+)))))
  ([x o- o+] (if (>= x 0)
               (normal x o+)
               (normal x o-))))

#_(defn nonbreak-acceptability [cwi x tw o- o+]
  (if (< tw (+ cwi x))
    (acc-distribution cwi x o- o+)
    1))

#_(defn explore-paths [i [b-path n-path] box tw o- o+]
  (let [bb (assoc b-path
                  :breakups (conj (:breakups b-path) i)
                  :current-acceptability (* (:current-acceptability b-path)
                                             (acc-distribution (:lenght box) (- tw) o- o+))
                  :current-nlw 0
                  :just-broke? true)
        bn (assoc b-path
                  :current-acceptability (* (:current-acceptability b-path)
                                            (unbreak-acceptability box b-path tw o- o+))
                  :current-nlw (+ (:current-nlw b-path) (:lenght box))
                  :just-broke? false)
        nb (assoc n-path
                  :breakups (conj (:breakups n-path) i)
                  :current-acceptability (* (:current-acceptability n-path)
                                          (acc-distribution (:lenght box) (- (:current-nlw n-path) tw) o- o+))
                  :current-nlw 0
                  :just-broke? true)
        nn (assoc n-path
                  :current-acceptability (* (:current-acceptability n-path)
                                            (unbreak-acceptability box n-path tw o- o+))
                  :current-nlw (+ (:current-nlw n-path) 1 (:lenght box))
                  :just-broke? false)]
    (println (:content box) "\n" bb "\n" bn "\n" nb "\n" nn "\n")
    [(max-key :current-acceptability bb nb) (max-key :current-acceptability bn nn)]))

#_(defn viterbi [text tw o- o+]
  (let [bxs (boxify text)]
    (loop [i 2
           boxes (next bxs)
           paths [(->Path [0 1] 0 (acc-distribution (:lenght (first bxs)) (- tw) o- o+) true)
                  (->Path [0  ] (:lenght (first bxs)) 1 true)]]
     (if (empty? boxes)
       paths
       (recur (inc i) (next boxes) (explore-paths i paths (first boxes) tw o- o+))))))
