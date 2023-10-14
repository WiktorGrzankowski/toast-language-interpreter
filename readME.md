# TOAST
Imperative programming language.

### Syntax
C-inspired with no significant changes.

### Types
Three standard ones: `int`, `string`, `bool`.

### Functions
Recursive, also nested, with static binding
```
int inner() {
    return 2;
}
int outer() {
    int inner() {
        return 5;
    }
    return inner();
}
```
Function outer() returns 5;
In Toast language every function must return a value.

### Execution
Toast does not require main() function, code is executed like in Python.

### Arithmetics
Standard ones, including division and modulo with catching runtime errors.

### Output
Function `Print` is used to write onto the standard output. `Print` takes as arguments all standard data types.

### Comments
Three kinds are allowed
```
# comment
// comment
/* co
mment */
```
### Break, continue
As in other languages.

### How to run it?
In the main folder type `make` and then the interpreter will be able to execute programs
typed as `./interpreter program` or `cat program | ./interpreter`.

