int sum(int byVal, int &byRef) {
    byVal++;
    byRef--;
    return byVal + byRef;
}

int v = 1;
int r = 1;
int s = sum(v, &r);
Print(v); // 1
Print(r); // 0