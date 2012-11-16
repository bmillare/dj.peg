# dj.peg 0.2.0

A Ring inspired (aka functional and composable) parsing expression
grammar (PEG) library.

A while back I wrote a PEG generator. Since it was buggy, I've
completely rewritten it and also tried to write it psuedo literate
programming (LP) style, in that I try to make it more of a story. I
don't rely on any LP tools, its just heavily commented and should flow
linearly. So if you never knew about PEGs before and want to dive into
it, this would be a good opportunity to check it out.

A quick example:

    (require [dj.peg :as peg])

    (let [num (peg/alt (peg/t #"\d+") #(Integer/parseInt %))
          whitespace (peg/t #"\s+")
          triplet (peg/s num whitespace num whitespace num)]
      (peg/parse triplet "3 44 2theremaininginput"))

    ;;user=> {:result [3 " " 44 " " 2] :unconsumed-input "theremaininginput"}

API summary:

t: token

s: sequence

*: star

+: plus

?: opt

|: choice

!?: not?

&?: and?

alt: alter-result


# Author

Brent Millare
brent.millare@gmail.com

# License

Copyright (c) Brent Millare. All rights reserved. The use and
distribution terms for this software are covered by the Eclipse Public
License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
be found in the file epl-v10.html at the root of this distribution. By
using this software in any fashion, you are agreeing to be bound by
the terms of this license. You must not remove this notice, or any
other, from this software.