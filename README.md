# Kaleidoscope
JIT compiler for the toy language Kaleidoscope. Based on a [tutorial](http://www.stephendiehl.com/llvm/#chapter-1-introduction), but updated for LLVM 6.0.

# Setup
You will need GHC 7.8 or newer as well as LLVM 6.0.

## Building with Stack
First

```$ stack build```

Then you can run the REPL with

```$ stack exec ks```

You can also compile and run specific files with

```$ stack exec ks <filename>```

For example

```$ stack exec ks examples/add.k```

# REPL Usage
You will be greeted by a command prompt that says `read> `. Enter in the Kalediscopoe code, ending with a `;`, and press enter to run the command. To exit, press ctrl-d.

Example usage
```
ready> def foo(a b) a*a + 2*a*b + b*b;
; ModuleID = 'my cool jit'

define double @foo(double %a, double %b)  {
entry:
  %0 = fmul double %a, %a
  %1 = fmul double %a, 2.000000e+00
  %2 = fmul double %1, %b
  %3 = fadd double %0, %2
  %4 = fmul double %b, %bncodeException
  %5 = fadd double %4, %3
  ret double %5
}

ready> def bar(a) foo(a, 4.0) + bar(31337);
define double @bar(double %a) {
entry:
  %0 = alloca double
  store double %a, double* %0
  %1 = load double* %0
  %2 = call double @foo(double %1, double 4.000000e+00)
  %3 = call double @bar(double 3.133700e+04)
  %4 = fadd double %2, %3
  ret double %4
}
```

# Current Functionality
Can only convert Kalediscope code into LLVM IR, and right now it throws an `EncodeException` when trying to use a function.
