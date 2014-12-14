(ns lab1.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as string])
  (:require [clojure.tools.cli :refer [cli]])
  (import java.lang.Math))

(defn get_dot_potential
  [p
   points
   distance-fn
   alpha]
  (list
   (reduce + (map #(Math/exp (- (* alpha (distance-fn % p)))) points))
   p))

(defn get_potentials
  [points
   distance-fn
   alpha]
  (map #(get_dot_potential % points distance-fn alpha) points))

(defn update_potentials
  [potentials
   core
   beta
   distance-fn]
  (let [core-potential (first core)
        core-point (last core)]
    (map #(list
           (- (first %) (* core-potential (Math/exp (* (- beta) (distance-fn core-point (last %))))))
           (last %))
         potentials)))

(defn parse_str
  [str]
  (map #(Double/parseDouble %)
       (drop-last (string/split str #","))))

(defn read_file
  [file-name]
  (with-open [rdr (io/reader file-name)]
    (doall (map parse_str (line-seq rdr)))))

(defn hamming_distance
  [p1 p2]
  (reduce + (map #(if (= %1 %2) 0 1) p1 p2)))

(defn euclid_distance
  [p1 p2]
  (Math/sqrt (reduce + (map #(Math/pow % 2) (map - p1 p2)))))


(defn reject_core
  [potentials
   rejected-point]
  (map #(if (= rejected-point %)
          (list 0 (last rejected-point))
          %)
       potentials))

(defn clusterize
  [points
   distance-fn]
  (let [radius-a 3
        radius-b (* radius-a 1.5)
        alpha (/ 4 (Math/pow radius-a 2))
        beta (/ 4 (Math/pow radius-b 2))
        upper-threshold 0.5
        lower-threshold 0.15
        potentials (get_potentials points distance-fn alpha)
        first-core (apply max-key first potentials)
        first-core-potential (first first-core)]
    (loop [potentials (update_potentials potentials first-core beta distance-fn)
           cores (list (last first-core))]
      (let [new-core (apply max-key first potentials)
            new-core-potential (first new-core)
            new-core-point (last new-core)]
        (cond
         (> new-core-potential (* upper-threshold first-core-potential))
           ;new center
           (recur (update_potentials potentials new-core beta distance-fn) (conj cores new-core-point))
         (< new-core-potential (* lower-threshold first-core-potential))
           ;end
           cores
         :else
           (let [dmin (apply min (map #(distance-fn new-core-point %) cores))]
             (if (>= (+ (/ dmin radius-a) (/ new-core-potential first-core-potential)) 1)
               ;new center
               (recur (update_potentials potentials new-core beta distance-fn) (conj cores new-core-point))
               ;reject new-core 
               (recur (reject_core potentials new-core) cores))))))))

(defn -main
  [& args]
  (let [[opts args] (cli args ["-f" "--file"   :default "resources/butterfly.txt"]
                             ["-e" "--euclid"]
                             ["-h" "--hamming"])]
    (if (:euclid opts)
      (println (clusterize (read_file (:file opts)) euclid_distance))
      (println (clusterize (read_file (:file opts)) hamming_distance)))))
