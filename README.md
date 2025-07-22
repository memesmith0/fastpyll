# fastpyll
this is a very trivial way to take any programming language that can be represented as an abstract syntax tree and force it to have its syntax be written in s expressions.

i can not fathom why previous programmers in lisp history have not done this (or have done this with half assed measures and failed).

humanity is not smart enough to understand this repository. my hope is that in hundreds of years humanity will look over this respository and realize their mistake

#edit

the ultimate goal of fastpyll is to pass it into somethign calle wisp which is a part of scheme that makes lisp use indentation for structure instead of parentehses just like python

edit:

this basically lets you rewrite python3 as a lisp. there should be little advantage to using this over regular python except that it has a lisp notation.

as I write this in the year of our lord 2025 the fastest way to use this is with hand written optimions in cython with something called pure python mode which is a party of cython that lets you write cython using only python. so basically since faspyll can write regular python then fastpyll can be used to hand write cython optimizations that can give up to a 30x boost to the underlying pythong code (at the expense of work by the programmer). without that extra work you should only get about a 10-40% increase in speed by using cython. this of course is all written in a lisp notation.

edit:

I've added fastpyll_c which is a version of fastpyll that compiles from python directly to c for extra speed. in order to make fastpyll_c work more like python I plan on recommending https://www.hboehm.info/gc/ plus a foreign functions interface to python
