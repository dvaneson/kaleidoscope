# Kaleidoscope
JIT compiler for the toy language Kaleidoscope. Based on a [tutorial](http://www.stephendiehl.com/llvm/#chapter-1-introduction), but updated for LLVM 6.0.

# Setup
You will need GHC 7.8 or newer as well as LLVM 6.0.

## Building with Stack
First, to build the executable do

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
ready> 4+5;
; ModuleID = 'my cool jit'
source filename = "<string>"

define double @main() {
entry:
  ret double 9.000000e+00
}

ready> def foo(a b) a*a + 2*a*b + b*b;
; ModuleID = 'my cool jit'

define double @foo(double %a, double %b)  {
entry:
  %0 = fmul double %a, %a
  %1 = fmul double 2.000000e+00, %a
  %2 = fmul double %1, %b
  %3 = fadd double %0, %2
  %4 = fmul double %b, %b
  %5 = fadd double %3, %4
  ret double %5
}

ready> def bar(a) foo(a, 4.0) + bar(31337);
define double @bar(double %a) {
entry:
  %0 = call double @foo(double %a, double 4.000000e+00)
  %1 = call double @bar(double 3.133700e+04)
  %2 = fadd double %0, %1
  ret double %2
}
```

# Kalediscope files
There are several example files in the `examples` directory. 
  - `add.k` just adds two numbers together, 
  - `extern.k` tests the usage of external functions. The JIT has some functions already defined in its address space, such as `sin` and `cos`. It is also possible to write C functions that can be found and executed with the LLVM JIT. For demonstration the C function `putchard`, whose implementation is in `cbits/putchard.c`, prints an ASCII character to the console. So, `extern.k` first declares the external functions `sin`, `cos`, and `putchard`. Then, it should print an 'x' followed by a newline, and lastly print the result of `sin(1)+cos(0.5)`.
  - `fn_call.k` tests the creation and use of functions. Also tests functions calling other functions in the module as well as external functions.
  - `ifthen.k` tests if/then/else functionality, and that nested if/then/else statements parse correctly.
  - `for.k` tests for loop functionality, and that nested for loops parse correctly. Uses `putchard` to print out newlines and stars.

# Current Functionality
Convert Kalediscope code into LLVM IR, then if a function named "main" exists, it is executed using JIT compilation.


