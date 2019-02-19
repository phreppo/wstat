module WhilePrograms 
    (
        factorial,
        expon,
        fibonacci,
        modul,
        empty_state
    )
where

-- x!
-- https://coolconversion.com/math/factorial/_700_factorial%3F
-- https://coolconversion.com/math/factorial/What-is-the-factorial-of_7000_%3F
factorial = "y:=1; while not (x = 1) do y := y*x; x := x-1"

--  x ^ y
expon = "accumulator := 1; for i:=1 to y do accumulator:=accumulator*x"

-- fib(x). fn1 = fib(n-1), fn2 = fib(n-2)
fibonacci = "fn1 := 1; fn2 := 0; for i:=2 to x do (fn := fn1+fn2; fn2 := fn1; fn1 := fn)"

-- x % y
modul = "r := x; while r >= y do r := r - y "

empty_state = "[]"

s1 = "[x -> 1]"

s2 = "[x -> 1, y->1]"