def foo(a b) a*a + 2*a*b + b*b;

extern cos(x);
def bar(a) foo(a, 4.0) + foo(5, a) + cos(a);

foo(1,2)+bar(3);
