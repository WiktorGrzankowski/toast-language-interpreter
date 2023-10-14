int fun() {
    return 12;
}

int x = fun();
Print(x); // 12

int factorial(int z) {
    if (z > 1) {
        return z * factorial(z - 1);
    }
    return z;
}
Print(factorial(5));