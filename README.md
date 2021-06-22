# lai

A compiled programming language.

## Example program
```
int terms = 10;

fn fib(int n) -> int {
    if (n <= 1) {
        return n;
    }

    return fib(n-1)+fib(n-2);
}


print(fib(terms));
```

## Features
* Static types
