(ns alphabet-cipher.coder
  (:require [clojure.string :as str]))

(def alphabet (mapv char (range (int \a) (inc (int \z)))))

(defn indexed-alphabet [alphabet]
  (into {} (map #(vector %1 %2) alphabet (range))))

(defn caesar-cipher [offset]
  (->> (cycle alphabet)
       (drop offset)
       (take (count alphabet))
       vec))

(defn cipher-letter [keyword-letter message-letter]
  (let [kl-idx ((indexed-alphabet alphabet) keyword-letter)
        ml-idx ((indexed-alphabet alphabet) message-letter)]
    (get (caesar-cipher ml-idx) kl-idx)))

(defn decode-letter [keyword-letter message-letter]
  (let [kl-idx ((indexed-alphabet alphabet) keyword-letter)
        caesar (caesar-cipher kl-idx)
        letter-idx ((indexed-alphabet caesar) message-letter)]
    (get alphabet letter-idx)))

(defn encode [keyword message]
  (apply str (map cipher-letter (cycle keyword) message)))

(defn decode [keyword message]
  (apply str (map decode-letter (cycle keyword) message)))

(defn sub-keyword [message cipher key idx]
  (let [res (subs key 0 idx)]
    (if (= cipher (encode res message))
      res
      false)))

(defn min-keyword [key message cipher]
  (some (partial sub-keyword message cipher key)
        (range 0 (count message))))

(defn decipher [cipher message]
  (min-keyword (apply str (map decode-letter message cipher))
               message
               cipher))

