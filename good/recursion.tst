int f(int a) {
    if (a > 5) {
        Print(a*a);
        return f(a-1);
    }
    return a;
}

Print(f(10));