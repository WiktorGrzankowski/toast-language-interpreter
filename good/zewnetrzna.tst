int ff() {
    return 10000000;
}
int f(int a) {
    int x = 2;
    int ff() {
        int x = 99;
        return x;
    }
    int fff() {
        return x;
    }
    return ff() + fff();
}

Print(f(3));