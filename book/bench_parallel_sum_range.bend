leaf = λval λn λl (l val)
node = λlft λrgt λn λl (n lft rgt)

height = 7
main = (sum (make height 0))

make = λh switch h { 0: make_leaf _: (make_node h-1) }
make_leaf = λn (leaf n)
make_node = λh-1 λn (node (make h-1 (* 2 n)) (make h-1 (+ 1 (* 2 n))))

sum = λt (t sum_node sum_leaf)
sum_node = λlft λrgt (+ (sum lft) (sum rgt))
sum_leaf = λval val
