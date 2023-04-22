int early = 5000;
int e() {
    return early + 2;
}

int f(int z) {
    int y = 420;
    return y;
}

int f2() {
    return f(2) + 1;
}

#int y = 69;
#int x = f2();

#Print(x);
#Print(y);
#Print(f(2));
Print(e());
