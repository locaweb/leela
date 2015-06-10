;; Copyright 2015 (c) Diego Souza <dsouza@c0d3.xxx>
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns leela.storaged.cassandra.config)

(def sequence-table "sequence")
(def sequence-blk-table "sequence_blk")
(def sequence-idx-table "sequence_idx")

(def bitmap-table "bitmap")
(def bitmap-idx-table "bitmap_idx")

(def metrics-table "metrics")
(def metrics-idx-table "metrics_idx")

(def metrics-trigger "metrics_trigger")
(def sequence-trigger "sequence_trigger")
