def foo(a b)
  if a < b then
    if b < 0 then
      0
    else
      b
  else
    a;

def fib(x)
  if x < 3 then
    1
  else
    fib(x-1) + fib(x-2);

foo(fib(10), fib(5));

