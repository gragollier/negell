# Negell
*A Haskell Negatron interpreter*

## Negatron
Negatron is a somewhat esoteric language from my compilers class that is essentially a subset of C except it doesn't support addition or support the use of `true`. Instead of defining the full spec of the language here, I will instead give a few of the important features of Negatron. Negatron is statically typed, statically scoped, has pass by value semantics, and only supports the types of `int` and `bool` for variables (`output` statements accept strings enclosed in double quotes e.g. `"Hello, world"`). Negatron programs start with an invocation of a `main` function. Additionally, Negatron has builtins for reading from and writing to the console thorough `output` and `input` respectively.

### Example Negatron Program
Bellow is Hello World in Negatron, for more examples look at the `.nega` files in the [./tests](./tests) folder.
```c
void main() {
    output "Hello, world!";
}
```

## The Interpreter
Negell itself uses Alex and Happy to lex and parse input programs into an AST. In the future this may be swapped out for something like parsec, but I initially wanted to focus more on the evaluation phase of the interpreter and I was already familiar with lex and bison so Alex and Happy were easy to pick up. Evaluation is accomplished by a single pass over the AST initially starting with the `main` function and collecting state changes from there.

## Usage
This project uses [cabal](https://www.haskell.org/cabal/) for project management. TL;DR is use `cabal run` to build and run Negell against `test.nega` and `cabal build` to only build the Negell binary.

### Running Negell
Currently Negell optionally takes one command line argument which is the path of the Negatron source file to execute, if no filename is provided it's assumed to be `test.nega`. 

### Testing Negell
Testing is currently accomplished by using a built version of Negell and comparing program output with expected output. To run the full suite of tests use the included shell script by running `./test.sh`.

## Known Issues and Todos
* There is little semantic checking of any kind. Badly typed or named programs may either crash or have undefined behavior
* Eval needs to be cleaned up as it's currently very messy
* Remove use of `unsafePerformIO` in Eval and instead trickle IO back up to main
* Tests are currently run outside of cabal and it would be nice if they used `cabal test` instead
