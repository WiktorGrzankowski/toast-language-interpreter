int z = -1;
int f(int &z, int aaa) {
    z++;
    return z;
}

int x = 50;
int y = 1;
Print(f(&x, y));
Print(x);
Print(z);