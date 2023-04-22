int z = -1;
int f(int &z) {
    z++;
    return z;
}

int x = 50;
Print(f(&x));
Print(x);
Print(z);