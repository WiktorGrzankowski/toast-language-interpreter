// should be an error, because argument should be passed by reference
int f(int &z) {
    return z;
}

int x = f(1);
Print(x);