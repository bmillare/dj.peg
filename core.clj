(ns peg-simple.core
  (:require [clojure.contrib.str-utils2 :as str-utils2]))

(defn make-terminal-parser [terminal]
  (fn [form]
    (when (= terminal (first form))
      (rest form))))

(defn make-non-terminal-parser [& expressions])

(defn peg-sequence [form & all-parse-expressions]
  (let [rest-form ((first all-parse-expressions) form)]
    (when rest-form 
      (if-let [rest-parse-expressions (next all-parse-expressions)] 
	(apply peg-sequence rest-form rest-parse-expressions)
	rest-form))))

(defn ordered-choice [form & all-parse-expressions]
  (if-let [rest-form ((first all-parse-expressions) form)]
    rest-form
    (when-let [rest-parse-expressions (next all-parse-expressions)]
      (apply ordered-choice form rest-parse-expressions))))

(defn zero-or-more [form parse-expression]
  (let [rest-form (parse-expression form)]
    (if (not rest-form)
      form
      (recur rest-form parse-expression))))

(defn one-or-more [form parse-expression]
  (when-let [rest-form (parse-expression form)]
    (zero-or-more rest-form parse-expression)))

(defn optional [form parse-expression]
  (let [rest-form (parse-expression form)]
    (if rest-form
      rest-form
      form)))

(defn and-predicate? [form parse-expression]
  (when-let [rest-form (parse-expression form)]
    form))

(defn not-predicate? [form parse-expression]
  (let [rest-form (parse-expression form)]
    (when-not rest-form
      form)))

(defn test-stuff []
  (let [h (make-terminal-parser \h)
	k (make-terminal-parser \k)
	z (make-terminal-parser \z)
	form "hkzabc"]
    (println (ordered-choice form z k h))
    (println (not-predicate? form k))
    (println (and-predicate? form h))
    (println (peg-sequence form h k z))
    (println (peg-sequence form (str-utils2/partial zero-or-more h) k z))))
