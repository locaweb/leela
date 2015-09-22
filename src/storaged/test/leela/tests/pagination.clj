(ns leela.tests.pagination
  (:require
   [clojure.set :refer [union]]
   [clojure.test :refer :all]))

(defn run-offset-pagination [fixture-fn offset-fn option-fn]
  (let [items    (max 1 (rand-int 1000))
        pages    (inc (rand-int items))
        limit    (quot items pages)
        store-fn (:store-fn option-fn)
        fetch-fn (:fetch-fn option-fn)
        mkrow-fn (:mkrow-fn option-fn)]
    (doseq [k (range items)]
      (store-fn k))
    (doseq [page (range pages)]
      (let [offset (offset-fn items limit page)
            fixture (fixture-fn offset limit)]
        (is (= (count (map mkrow-fn fixture)) (count (fetch-fn offset limit))))
        (is (every? true?
                    (map =
                         (map mkrow-fn fixture)
                         (fetch-fn offset limit))))))))

(defn- token-pagination-loop [view-fn last-fn fetch-fn limit]
  (loop [rset #{}
         next nil]
    (let [pack   (if (nil? next)
                   (fetch-fn (inc limit))
                   (fetch-fn next (inc limit)))
          token  (last-fn (butlast pack))
          v-pack (map view-fn pack)]
      (is (every? (partial (complement contains?) rset) v-pack))
      (if (> (count pack) limit)
        (do
          (when (nil? token) (throw (RuntimeException. "token is nil")))
          (recur (union rset (set (butlast v-pack))) token))
        (union rset (set v-pack))))))

(defn run-token-pagination [option-fn]
  (let [items    10; (max 1 (rand-int 1000))
        pages    (inc (rand-int items))
        limit    (quot items pages)
        view-fn  (:view-fn option-fn)
        last-fn  (:last-fn option-fn)
        fetch-fn (:fetch-fn option-fn)
        store-fn (:store-fn option-fn)
        mkrow-fn (:mkrow-fn option-fn)]
    (doseq [k (range items)]
      (store-fn k))
    (is (= (token-pagination-loop view-fn last-fn fetch-fn limit)
           (set (map mkrow-fn (range items)))))))

(defmacro deftest-pagination-token [name option-fn]
  `(deftest ~name
     (run-token-pagination ~option-fn)))

(defmacro deftest-pagination-desc [name option-fn]
  (letfn [(offset-fn [items ppage page]
            (- items (* ppage page)))
          (fixture-fn [offset limit]
            (reverse (range (max 0 (- offset limit)) offset)))]
    `(deftest ~name
       (run-offset-pagination ~fixture-fn ~offset-fn ~option-fn))))

(defmacro deftest-pagination-asc [name option-fn]
  (letfn [(offset-fn [items ppage page]
            (dec (* ppage page)))
          (fixture-fn [offset limit]
            (range (inc offset) (inc (+ offset limit))))]
    `(deftest ~name
       (run-offset-pagination ~fixture-fn ~offset-fn ~option-fn))))
