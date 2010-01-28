(ns simple-peg.core
  "simple-peg is designed to be a straightforward implementation of a
parser generator that takes a parsing expression grammar (PEG)
and generates a parser

The goals are to be (in order of importance), simple (so I can develop it),
then later flexible, then fast/featureful. It should mesh well with clojure
when possible. So for example, I opted for implementing a prefix style notation
when defining the PEG

simple-peg ultimately operates only on sequences but many things can be represented
as sequences

TODO:
add let-like binding form for describing named expressions all together
    -debug cyclicly defined PEGs
add ability to execute code as parsing
add default ability to output AST
PUBLISH
"
  (:use clojure.contrib.macro-utils))

(defn- peg-sequence [form & all-parse-expressions]
  (println "s" form)
  (let [rest-form ((first all-parse-expressions) form)]
    (when rest-form 
      (if-let [rest-parse-expressions (next all-parse-expressions)] 
	(apply peg-sequence rest-form rest-parse-expressions)
	rest-form))))

(defn- ordered-choice [form & all-parse-expressions]
  (println "o-c" form)
  (if-let [rest-form ((first all-parse-expressions) form)]
    rest-form
    (when-let [rest-parse-expressions (next all-parse-expressions)]
      (apply ordered-choice form rest-parse-expressions))))

(defn- zero-or-more [form parse-expression]
  (println "z-o-m" form)
  (let [rest-form (parse-expression form)]
    (if (not rest-form)
      form
      (recur rest-form parse-expression))))

(defn- one-or-more [form parse-expression]
  (println "o-o-m" form)
  (when-let [rest-form (parse-expression form)]
    (zero-or-more rest-form parse-expression)))

(defn- optional [form parse-expression]
  (println "o" form)
  (let [rest-form (parse-expression form)]
    (if rest-form
      rest-form
      form)))

(defn- and-predicate? [form parse-expression]
  (println "a-p?" form)
  (when-let [rest-form (parse-expression form)]
    form))

(defn- not-predicate? [form parse-expression]
  (println "n-p?" form)
  (let [rest-form (parse-expression form)]
    (when-not rest-form
      form)))

(defn- make-terminal-parser [terminal]
  (println "make-terminal-parser" terminal)  
  (fn [form]
    (when (= terminal (first form))
      (rest form))))

(defn- make-non-terminal-many-parser [operator all-parse-expressions]
  (println "make-non-terminal-many-parser" operator)
  (fn [form]
    (apply operator form all-parse-expressions)))

(defn- make-non-terminal-parser [operator parse-expression]
  (println "make-non-terminal-parser" operator)  
  (fn [form]
    (operator form parse-expression)))

(defn make-parser-aux [expression-form]
  (println "make-parser-aux" (if (seq? expression-form)
			       (#{::sequence ::ordered-choice ::zero-or-more ::one-or-more
	      ::optional ::and-predicate? ::not-predicate} (first expression-form))
			       expression-form))
  (if (and (seq? expression-form)
	   (#{::sequence ::ordered-choice ::zero-or-more ::one-or-more
	      ::optional ::and-predicate? ::not-predicate} (first expression-form)))
    (case (first expression-form)
	  ::sequence (make-non-terminal-many-parser peg-sequence
				      (map make-parser-aux (rest expression-form)))
	  ::ordered-choice (make-non-terminal-many-parser ordered-choice
				      (map make-parser-aux (rest expression-form)))
	  ::zero-or-more (make-non-terminal-parser zero-or-more
				      (make-parser-aux (second expression-form)))
	  ::one-or-more (make-non-terminal-parser one-or-more
				      (make-parser-aux (second expression-form)))
	  ::optional (make-non-terminal-parser optional
				      (make-parser-aux (second expression-form)))
	  ::and-predicate? (make-non-terminal-parser and-predicate?
				      (make-parser-aux (second expression-form)))
	  ::not-predicate? (make-non-terminal-parser not-predicate?
				      (make-parser-aux (second expression-form))))
    (make-terminal-parser expression-form)))

(defmacro s [& rest-args]
  `(list ::sequence ~@rest-args))
(defmacro o-c [& rest-args]
  `(list ::ordered-choice ~@rest-args))
(defmacro z-o-m [arg]
  `(list ::zero-or-more ~arg))
(defmacro o-o-m [arg]
  `(list ::one-or-more ~arg))
(defmacro o [arg]
  `(list ::optional ~arg))
(defmacro a-p? [arg]
  `(list ::and-predicate? ~arg))
(defmacro n-p? [arg]
  `(list ::not-predicate? ~arg))
(defn ds [rest-args]
  (apply list ::sequence rest-args))
(defn do-c [rest-args]
  (apply list ::ordered-choice rest-args))

(defmacro make-parser [bindings]
  "Using let like syntax, passes final PEG form into make-parser-aux"
  (assert (and (vector? bindings)
	       (even? (count bindings))))
  (let [gensym-bindings (map (fn [name]
			       (vector name (gensym name)))
			     (take-nth 2 bindings))
	gensym-map (into {} gensym-bindings)]
    `(symbol-macrolet [~@(apply concat (map (fn [[name proxy-name]]
					      `(~name (~proxy-name)))
					    gensym-bindings))]
      (letfn [~@(map (fn [[name form]]
		       `(~(gensym-map name) [] (lazy-seq ~form)))
		     (partition 2 bindings))]
	(make-parser-aux (~(second (last gensym-bindings))))))))



(defn test1 []
  (let [h (make-terminal-parser \h)
	k (make-terminal-parser \k)
	z (make-terminal-parser \z)
	form "hkzabc"]
    (println (ordered-choice form z k h))
    (println (not-predicate? form k))
    (println (and-predicate? form h))
    (println (peg-sequence form h k z))))

(defn test2 []
  nil)

(defn test3 []
  (let [x (make-parser-aux (o-o-m 'a))]
    (x '(a b c d))))

(defn test4 []
  (let [x (make-parser-aux (s 'a (o-o-m (do-c '(b c)))))]
    (x '(a b c d))))

(defn test5 []
  (let [x (make-parser-aux (ds (seq "hello")))]
    (x "hellothere")))

(defn test6 []
  (let [x (make-parser [ProductOp (o-c \* \\)
		       SumOp (o-c \+ \-)
		       Num (o-c \a \b)
		       Value (o-c Num Sum)
		       Product (s Value (z-o-m (s ProductOp Value)))
		       Sum (s Product (z-o-m (s SumOp Product)))
		       Expr (s Sum ())])]
    (x '(\a \+ \b))))

(defn test7 []
  (let [x (make-parser [A (s \a (o A) \b)
			B (s \b (o B) \c)
			S (s (a-p? (s A \c)) (o-o-m \a) B (n-p? (o-c \a \b \c)))])]
    (x '(\a \b \c))))