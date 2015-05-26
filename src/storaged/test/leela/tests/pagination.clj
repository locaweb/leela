(ns leela.tests.pagination
  (:require
   [clojure.test :refer :all]))

(defn eq-not-nil [a b]
  (and (= a b) (not-any? nil? [a b])))

(defmacro deftest-pagination [name fixture-fn offset-fn option-fn]
  (let [store-fn (:store-fn option-fn)
        fetch-fn (:fetch-fn option-fn)
        mkrow-fn (:mkrow-fn option-fn)
        view-fn  (get option-fn :view-fn identity)]
    `(deftest ~name
       (let [items# 10 ; (max 1 (rand-int 1000))
             pages# 5 ; (+ 1 (rand-int items#))
             limit# (quot items# pages#)]
         (doseq [k# (range items#)]
           (~store-fn k#))
         (doseq [page# (range pages#)]
           (let [offset# (~offset-fn items# limit# page#)
                 fixture# (~fixture-fn offset# limit#)]
             (is (every? true?
                         (map #(eq-not-nil (~view-fn %1) (~view-fn %2))
                              (map ~mkrow-fn fixture#)
                              (~fetch-fn offset# limit#))))))))))

(defmacro deftest-pagination-desc [name option-fn]
  (letfn [(offset-fn [items ppage page]
            (- items (* ppage page)))
          (fixture-fn [offset limit]
            (reverse (range (max 0 (- offset limit)) offset)))]
    `(deftest-pagination ~name ~fixture-fn ~offset-fn ~option-fn)))

(defmacro deftest-pagination-asc [name option-fn]
  (letfn [(offset-fn [items ppage page]
            (dec (* ppage page)))
          (fixture-fn [offset limit]
            (range (inc offset) (inc (+ offset limit))))]
    `(deftest-pagination ~name ~fixture-fn ~offset-fn ~option-fn)))
