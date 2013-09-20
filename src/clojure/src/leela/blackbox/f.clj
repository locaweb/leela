(ns leela.blackbox.f)

(defmacro forever [& body]
  `(forever-with (fn [] true) ~@body))

(defmacro forever-with [check & body]
  `(while (~check) ~@body))

(defmacro supervise [& body]
  `(supervise-with (fn [] true) ~@body))

(defmacro supervise-with [check & body]
  `(forever-with ~check
    (try
      ~@body
      (catch Exception _#))))

(defn random-string [bits]
  (.toString (java.math.BigInteger. bits (java.security.SecureRandom.)) 32))

(defn utf8-string [bytes]
  (String. bytes "UTF-8"))
