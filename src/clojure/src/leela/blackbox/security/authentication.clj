;; This file is part of Leela.
;;
;; Leela is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Leela is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Leela.  If not, see <http://www.gnu.org/licenses/>.

(ns leela.blackbox.security.authentication
  (:use     [clj-time.core   :only [minutes
                                    ago
                                    from-now
                                    within?
                                    interval]]
            [clj-time.coerce :only [to-long
                                    from-long]])
  (:require [pandect.core   :as auth]))

(defn signature
  "With a message and secret provided by the caller,
  generate the MD5 HMAC hash."
  [message secret]
  (auth/md5-hmac message secret))

(defn valid-time?
  "Given a timestamp, a specific moment on time and
  a window around this moment,
  checks if the timestamp was within this interval."
  [timestamp now window]
  (within?
    (interval
      (from-long (- (to-long (now)) (* window 60000)))
      (from-long (+ (to-long (now)) (* window 60000))))
    timestamp))

(defn valid-nonce?
  "Given a nonce, checks if it's valid."
  [nonce]
  (true))
