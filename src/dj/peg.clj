(ns dj.peg
  (:refer-clojure :exclude [* +]))
;; api:
;; t, s, *, +, ?, /, !?, &?, alt

;; Note: This file is written in a pseudo literate programming (LP)
;; style. Instead of relying on the LP tools to expand and reorganize
;; the code, the reader will need to navigate the code using the
;; search function in their text editor :)

;; Summary:

;; This a functional Parser Expression Grammar library designed for
;; writing composable parsers.

;; Advantages:

;; PEG parsers use recursion and thus are strictly more powerful than
;; regular expressions. For example, PEG parsers can parse nested
;; parentheses but regular expression can not. Because the choice
;; operator creates a preference over two paths, PEG parsers are never
;; ambiguous. So for example, they will always be able to parse the
;; "dangling else" problem found in C, C++ and Java. Utilizing
;; functional programming techniques, one can write small, composable
;; parsers, and thus eliminate the need to define grammars in a
;; separate format and call large parser generators such as lex and
;; yacc. (See parser combinators from Haskell).

;; Disadvantages:

;; Without memoization, PEG parsers can have asymptotic
;; performance. With memoization, they can consume much more memory
;; but will run in linear time. For many cases in practice this is not
;; an issue. Instead, a more important concern is the problem of
;; indirect left recursion. This library makes no attempt to solve
;; these problems automatically. The user of the library should be
;; aware of these issues and rework their grammars as
;; appropriate. Another side effect of using recursive functions is
;; the consumption of stack (no assumption is made that the user is
;; using clojure on a tail call optimizing version of the JVM). This
;; library constrains memory stack usage with clojure's trampolines.

;; Components:

;; To understand the programming model, there are 4 types of functions
;; in this library you need to understand:

;; 1. Parsers Generators
;; (parsers* | args) -> parser
;; 2. Parsers
;; (input succeed fail) -> call to succeed or fail
;; 3. Success and Fail continuation functions
;; (result remaining-input)
;;  -> call to parsers (usually with closed over values) [delegation]
;;  -> call to another succeed or fail function [aggregation]
;;  -> aggregates results (terminal)

;; Note: To reduce the number of continuations managed, we assume
;; 'failures' will never consume input and 'successes' may or may not
;; consume input

;; 4. Continuation wrappers
;; (parser args*) -> modified parser

;; Trampoline wrappers, which are also included in this library, are
;; just convenience functions for calling the parsers.

;; Overview:

;; Token is the model terminal parser.

;; The parser generators/parsers are the 'bread and butter' for users
;; of this library, and which are the functions the users will call to
;; construct grammars.

;; alt is the default continuation wrapper that lets users do useful
;; work after parsing in an inline style. Users of this library will
;; need to interleave this to different parts of their sub-parsers.

;; Finally, parse is the main invocation point that wraps a trampoline
;; call and provides default success and fail continuations.

;; Components

;; token
(defn t
  ;; This is the first example parser generator. Given a regular
  ;; expression, it returns a parser that will succeed if there is a
  ;; match (see .lookingAt to understand exactly what constitutes a
  ;; match), else it will fail. To succeed or fail, means to call
  ;; succeed or call fail. This is continuation style programming, and
  ;; simplifies control flow. The parser that token returns, is called
  ;; a terminal parser, since it does not delegate to other parsers
  ;; that conform to the function contracts. This stops the recursion
  ;; and thus 'terminates' the parsing. In addition, what it means for
  ;; a java re matcher to succeed and fail, must be translated to the
  ;; continuation convention.
  "returns parser that looks for token that matches re"
  [^java.util.regex.Pattern re]
  (fn [input succeed fail]
    (let [m (re-matcher re input)]
      ;; Translation is accomplished using .lookingAt, which attempts
      ;; to match the input sequence, starting at the beginning of the
      ;; region, against the pattern. It does not require the whole
      ;; input match, but only from the start a match can be found. It
      ;; returns true if it succeeds, else false. This information is
      ;; tracked and then appropriate continuation is called.
      (if (.lookingAt m)
	;; Another important aspect of parsers is how they manage the
	;; results of the parsing. On success, this parser will return
	;; the matched string using .group. What input was consumed
	;; and what remains to be consumed must be managed.
	
	(succeed (.group m) ;; .group returns the matched string
		 (subs input (.end m))) ;; .end returns the index of the end character
        ;; for error reporting, the regex that failed is returned
	(fail re input)))))

;; NOTE: On writing terminal parsers. In the above example, a string,
;; instead of being though of a sequence of characters, it can instead
;; be thought of as a sequence of regular expression matches. This
;; abstraction has the advantage that you can usually parse text more
;; efficiently and express common idioms in regular expressions.

;; This library is general in that if you write your own terminal
;; parsers on different input types like number sequences, the
;; non-terminal parsers should "just work" on them.

;; As mentioned before, functional parsers can consume a lot of stack
;; on non tail call optimizing compilers. This PEG library uses
;; clojure's trampolines to limit consumption of the memory
;; stack. Instead of directly calling the function, a closure that
;; calls the function, is returned. When you use trampoline to invoke
;; the parsers, trampoline will automatically call the next closure
;; returned by the parser until the closures no longer returns another
;; closure. This will happen when the highest most continuation
;; function is called.

;; To make it more clear that a closure is returned for trampoline,
;; and not a parser or continuation, the macro, bounce, that does the
;; closure wrapping, was written. Semantically, this is a good name,
;; since one bounces on the trampoline to make the function call.

(defmacro bounce
  "use instead of calling a function, this will create a closure for
  use with trampoline to avoid stackoverflows due to mutual recursion"
  [f & form]
  `(fn [] (~f ~@form)))

;; Although this does reduce our stack consumption, it does clutter
;; the code. As a compromise, bounce is called only on nonterminal
;; parsers. Its first usage is demonstrated in the first delegating
;; parser, seq

;; seq
(defn s
 "The seq parser generator returns a parser that succeeds only if all
 parsers succeed. The result that will be passed to succeed will be a
 vector of the results returned by all the other parsers."
  ([m n]
     (fn [input succeed fail]
       (bounce
	m
	input
	;; If m succeeds, check to see if the next parser succeeds.
	(fn [m-result m-rest-input]
	  (bounce
	   n
	   m-rest-input
	   (fn [n-result n-rest-input]
	     ;; The result is a vector of all the results
	     (succeed [m-result n-result] n-rest-input))
	   (fn [e-result _]
	     (fail e-result input))))
	fail)))
  ([m n & args]
     ;; The more than 2 parser case is tricky because the passed
     ;; result should be a flat vector and not nested.  The way
     ;; results are joined after seq is called needs to be changed
     ;; since the result is already a vector. Therefore, conj is
     ;; called
     (let [seq' (fn [m' n']
		   (fn [input succeed fail]
		     (bounce
		      m'
		      input
		      (fn [m'-result m'-rest-input]
			(bounce
			 n'
			 m'-rest-input
			 (fn [n'-result n'-rest-input]
			   (succeed (conj m'-result n'-result) n'-rest-input))
			 (fn [e-result _]
			   (fail e-result input))))
		      fail)))]
       (reduce seq' (s m n) args))))

;; choice
(defn |
  "returns a parser that calls succeed on the first succeeded parser"
  ([m n]
     (fn [input succeed fail]
       (bounce
	m
	input
	succeed
	(fn [_ _]
	  (bounce
	   n
	   input
	   succeed
	   fail)))))
  ([m n & args]
     (reduce | (| m n) args)))

;; star
(defn *
  "returns a parser that always succeeds on n number of calls to
  parser x on input"
  [x]
  (fn [input succeed fail]
    ;; Like in the seq case, state must be correctly accumulated. Here
    ;; on the first successful parsing, the state is put in a
    ;; vector. From then on, all successful parses gets conjed onto
    ;; that original vector.
    (letfn [(first-continue [old-result old-rest-input]
			    (bounce
			     x
			     old-rest-input
			     (fn [new-result new-rest-input]
			       (continue [old-result new-result] new-rest-input))
			     (fn [new-result new-rest-input]
			       (succeed [old-result] new-rest-input))))
	    (continue [old-result old-rest-input]
		      (bounce
		       x
		       old-rest-input
		       (fn [new-result new-rest-input]
			 (continue (conj old-result new-result) new-rest-input))
		       (fn [new-result new-rest-input]
			 (succeed old-result new-rest-input))))]
      (bounce
       x
       input
       first-continue
       (fn [_ _]
         (succeed nil input))))))

;; plus
(defn + [x]
  (fn [input succeed fail]
    (letfn [(first-continue [old-result old-rest-input]
			    (bounce
			     x
			     old-rest-input
			     (fn [new-result new-rest-input]
			       (continue [old-result new-result] new-rest-input))
			     (fn [new-result new-rest-input]
			       (succeed [old-result] new-rest-input))))
	    (continue [old-result old-rest-input]
		      (bounce
		       x
		       old-rest-input
		       (fn [new-result new-rest-input]
			 (continue (conj old-result new-result) new-rest-input))
		       (fn [new-result new-rest-input]
			 (succeed old-result new-rest-input))))]
      (bounce
       x
       input
       first-continue
       fail))))

;; Note that not? and and? do not consume input.

;; not?
(defn !?
  "negative lookahead, returns parser that parses without consuming
  input"
  [x]
  (fn [input succeed fail]
    (bounce
     x
     input
     (fn [result _]
       (fail result input))
     (fn [result _]
       (succeed result input)))))

;; and?
(defn &?
  "and lookahead, returns parser that parses without consuming input"
  [x]
  (fn [input succeed fail]
    (bounce
     x
     input
     (fn [result _]
       (succeed result input))
     (fn [result _]
       (fail result input)))))

;; opt
(defn ?
  "returns parser that optionally accepts input"
  [x]
  (fn [input succeed fail]
    (bounce
     x
     input
     succeed
     (fn [_ _]
       (succeed nil input)))))

(defn parse
;; This is the default trampoline wrapper. Use this function to invoke
;; a parser at the toplevel. Example: (peg/parse (peg/t #"\d+") "234")
  "calls the parser on input with default continuation functions. On
  success, returns a hash-map of the result and the remaining
  input. On failure, throws and exception with the current result and
  remaining input. Uses trampolines underneath to limit stack
  consumption. You can also supply your own succeed and fail
  continuation functions."
  ([parser input]
     (trampoline parser
		 input
		 (fn [result rest-input]
                   {:result result
                    :unconsumed-input rest-input})
		 (fn [result rest-input]
		   (throw (ex-info (str "Parse failed with result: "
                                        result)
                                   {:result result
                                    :unconsumed-input rest-input})))))
  ([parser input succeed fail]
     (trampoline parser
		 input
		 succeed
		 fail)))

;; The peg library takes some inspiration from Ring. The function alt
;; is like middleware in that it wraps the old parser, does some data
;; manipulation, and returns a new parser.

(defn alt
;; One good example of usage is, you want to parse a number, so you
;; write a token parser with (peg/t #"\d+"). You want the result to be
;; an actual number, so you wrap it with java's integer
;; parser. (peg/alt (peg/t #"\d+") #(Integer/parseInt %)) Now, when
;; you invoke it, succeed gets passed an Integer instead of a string.
  "returns a wrapped version of parser p that modifies result before
  passing it to the succeed function"
  [p result-alter-fn]
  (fn [input succeed fail]
    (bounce
     p
     input
     (fn [result rest-input]
       (succeed (result-alter-fn result) rest-input))
     fail)))
