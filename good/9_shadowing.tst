int x = 10;
int y = 2;
if (y > 1) {
    int x = 15;
    y = 4;
    Print(x); // local: 15
}
Print(x); // global: 10
Print(y); // global: 4

int a = 1000;
int f(int b) {
    int a = 1;
    return a + b;
}

int f2(int a) {
    return a;
}

Print(f(1)); // a is global, so: 2
Print(f2(1)); // a is an argument, so:1
