# Graph search with delimited continuations
Playing around with delimited continuations by defining graph search
 algortihms. I chose Racket for this task but I think you can do
pretty much the same with OCaml's effects.

If you are really interested in delimited continuations, you should
probably consider reading [this book](https://xavierleroy.org/control-structures/book/index.html) by Xavier Leroy.
You should also check out [the lecture notes and Rocq
mechanization](https://github.com/sweirich/pl-semantics-and-types) by
my advisor.


This repo is currently undocumented and I might write a tutorial on
delimited continuations based on the code here.

I've managed to define a simple dfs search for cycle detection and I plan to
"stress test" the generality of the method by implementing a few more
leetcode graph algorithms. I'm quite sick of leetcode problems and
this is my way of at least getting some joy out of it.
