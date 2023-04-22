int f() {
    int y = 420;
    return y;
}

int f2() {
    return f() + 1;
}

int y = 69;
int x = f2();

Print(x);
Print(y);